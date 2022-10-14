/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/ott/2013
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.event.Event;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.gui.components.MainPane;
import org.pdfsam.gui.components.dashboard.preference.PreferenceConfig;
import org.pdfsam.gui.components.notification.NotificationsContainer;
import org.pdfsam.gui.configuration.DashboardConfig;
import org.pdfsam.gui.configuration.PdfsamConfig;
import org.pdfsam.gui.configuration.PersistenceConfig;
import org.pdfsam.gui.configuration.ServicesConfig;
import org.pdfsam.gui.theme.Themes;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.injector.Injector;
import org.pdfsam.model.lifecycle.ShutdownEvent;
import org.pdfsam.model.lifecycle.StartupEvent;
import org.pdfsam.model.news.FetchLatestNewsRequest;
import org.pdfsam.model.premium.FetchPremiumModulesRequest;
import org.pdfsam.model.ui.SetActiveToolRequest;
import org.pdfsam.model.ui.SetLatestStageStatusRequest;
import org.pdfsam.model.ui.ShowStageRequest;
import org.pdfsam.model.ui.StageMode;
import org.pdfsam.model.ui.StageStatus;
import org.pdfsam.model.ui.workspace.LoadWorkspaceRequest;
import org.pdfsam.model.ui.workspace.SaveWorkspaceRequest;
import org.pdfsam.model.update.UpdateCheckRequest;
import org.pdfsam.service.news.NewsService;
import org.pdfsam.service.ui.StageService;
import org.pdfsam.ui.components.tool.RunButtonTriggerRequest;
import org.sejda.core.Sejda;
import org.sejda.impl.sambox.component.PDDocumentHandler;
import org.sejda.model.util.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.SplashScreen;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.core.context.StringPersistentProperty.THEME;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * PDFsam application
 *
 * @author Andrea Vacondio
 */
public class PdfsamApp extends Application {
    private static final Logger LOG = LoggerFactory.getLogger(PdfsamApp.class);

    private static final StopWatch STOPWATCH = new StopWatch();
    private Stage primaryStage;
    private List<String> rawParameters;
    private boolean clean;

    @Override
    public void init() {
        STOPWATCH.start();
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionLogger());
        rawParameters = getParameters().getRaw();
        System.setProperty(PDDocumentHandler.SAMBOX_USE_ASYNC_WRITER, Boolean.TRUE.toString());
        System.setProperty(Sejda.UNETHICAL_READ_PROPERTY_NAME, Boolean.TRUE.toString());
        System.setProperty(IOUtils.TMP_BUFFER_PREFIX_PROPERTY_NAME, "pdfsam");
        LOG.info("Starting...");
        clean = rawParameters.contains("--clean") || rawParameters.contains("-clean") || rawParameters.contains("-c");
        if (clean) {
            app().clean();
        }
        app().persistentSettings().get(StringPersistentProperty.LOCALE)
                .ifPresent(l -> eventStudio().broadcast(new SetLocaleRequest(l)));

        var workingPath = app().persistentSettings().get(StringPersistentProperty.WORKING_PATH)
                .filter(StringUtils::isNotBlank).map(Paths::get).filter(Files::isDirectory).orElse(null);
        app().runtimeState().workingPath(workingPath);
    }

    @Override
    public void start(Stage primaryStage) {
        this.primaryStage = primaryStage;
        initInjector(primaryStage);
        cleanIfRequired();

        primaryStage.setScene(initScene());
        primaryStage.getIcons().addAll(app().instancesOfType(Image.class));
        primaryStage.setOnCloseRequest(e -> Platform.exit());

        app().instance(WindowStatusController.class).setStage(primaryStage);
        app().instance(ApplicationTitleController.class).setStage(primaryStage);

        requestPremiumModulesDescriptionIfRequired();
        initActiveTool();
        loadWorkspaceIfRequired();
        escapeMnemonicsOnFocusLost();
        primaryStage.show();

        requestCheckForUpdateIfRequired();
        requestLatestNewsIfRequired();
        initSejda();
        closeSplash();
        STOPWATCH.stop();
        eventStudio().broadcast(new StartupEvent());
        LOG.info(i18n().tr("Started in {0}", DurationFormatUtils.formatDurationWords(STOPWATCH.getTime(), true, true)));
        new InputPdfArgumentsConsumer().accept(rawParameters);
    }

    private void initInjector(Stage primaryStage) {
        Injector.addConfig(new PdfsamConfig(getHostServices(), primaryStage));
        Injector.addConfig(new PersistenceConfig(), new ServicesConfig(), new PreferenceConfig(),
                new DashboardConfig());
        app().injector(Injector.start());
    }

    private void cleanIfRequired() {
        if (clean) {
            LOG.debug("Cleaning...");
            app().instance(NewsService.class).clear();
            app().instance(StageService.class).clear();
        }
    }

    private void initSejda() {
        var appBrand = app().instance(AppBrand.class);
        Sejda.CREATOR = appBrand.property(BrandableProperty.SHORT_NAME, "PDFsam Basic") + " v" + appBrand.property(
                BrandableProperty.VERSION, "UNKNOWN");
    }

    private void closeSplash() {
        Optional.ofNullable(SplashScreen.getSplashScreen()).ifPresent(SplashScreen::close);
    }

    private Scene initScene() {
        var mainPane = app().instance(MainPane.class);
        var notifications = app().instance(NotificationsContainer.class);

        StackPane main = new StackPane();
        StackPane.setAlignment(notifications, Pos.BOTTOM_RIGHT);
        StackPane.setAlignment(mainPane, Pos.TOP_LEFT);
        main.getChildren().addAll(mainPane, notifications);

        Scene mainScene = new Scene(main);
        initTheme(mainScene);
        mainScene.getAccelerators().put(new KeyCodeCombination(KeyCode.L, KeyCombination.SHORTCUT_DOWN),
                () -> eventStudio().broadcast(ShowStageRequest.INSTANCE, "LogStage"));
        mainScene.getAccelerators()
                .put(new KeyCodeCombination(KeyCode.Q, KeyCombination.SHORTCUT_DOWN), Platform::exit);
        mainScene.getAccelerators().put(new KeyCodeCombination(KeyCode.X, KeyCombination.SHORTCUT_DOWN),
                () -> eventStudio().broadcast(RunButtonTriggerRequest.INSTANCE));
        return mainScene;
    }

    private void initTheme(Scene scene) {
        app().runtimeState().subscribeThemedScene(scene);
        var theme = app().persistentSettings().get(THEME).orElse(null);
        app().runtimeState().theme(Themes.getOrDefault(theme));
    }

    private void requestCheckForUpdateIfRequired() {
        if (app().persistentSettings().get(BooleanPersistentProperty.CHECK_UPDATES)) {
            eventStudio().broadcast(new UpdateCheckRequest(false));
        }
    }

    private void requestLatestNewsIfRequired() {
        if (app().persistentSettings().get(BooleanPersistentProperty.CHECK_FOR_NEWS)) {
            eventStudio().broadcast(FetchLatestNewsRequest.INSTANCE);
        }
    }

    /**
     * This is a workaround for <a href="https://bugs.openjdk.java.net/browse/JDK-8238731">...</a>
     * <p>
     * We fire an ESC key pressed event when the windows looses focus to clear all the mnemonics. Not sure all the edge cases are taken into account, I guess we'll see if some user
     * reports something.
     */
    private void escapeMnemonicsOnFocusLost() {
        primaryStage.focusedProperty().addListener((obs, oldVal, newVal) -> {
            if (!newVal) {
                Event.fireEvent(primaryStage.getScene(),
                        new KeyEvent(KeyEvent.KEY_PRESSED, KeyEvent.CHAR_UNDEFINED, KeyCode.ESCAPE.getName(),
                                KeyCode.ESCAPE, false, false, false, false));
            }
        });
    }

    private void initActiveTool() {
        app().persistentSettings().get(StringPersistentProperty.STARTUP_MODULE).filter(StringUtils::isNotBlank)
                .ifPresent(tool -> {
                    LOG.trace("Activating startup tool '{}'", tool);
                    eventStudio().broadcast(new SetActiveToolRequest(tool));
                });
    }

    private void requestPremiumModulesDescriptionIfRequired() {
        if (app().persistentSettings().get(BooleanPersistentProperty.PREMIUM_MODULES)) {
            eventStudio().broadcast(FetchPremiumModulesRequest.INSTANCE);
        }
    }

    private void loadWorkspaceIfRequired() {
        app().persistentSettings().get(StringPersistentProperty.WORKSPACE_PATH).filter(StringUtils::isNotBlank)
                .map(Paths::get).filter(Files::exists).map(Path::toFile).map(LoadWorkspaceRequest::new)
                .ifPresent(eventStudio()::broadcast);
    }

    @Override
    public void stop() {
        LOG.info(i18n().tr("Closing PDFsam..."));
        var status = new StageStatus(this.primaryStage.getX(), this.primaryStage.getY(), this.primaryStage.getWidth(),
                this.primaryStage.getHeight(), StageMode.valueFor(this.primaryStage));
        eventStudio().broadcast(new SetLatestStageStatusRequest(status));
        saveWorkspaceIfRequired();
        eventStudio().broadcast(new ShutdownEvent());
        app().close();
    }

    private void saveWorkspaceIfRequired() {
        if (app().persistentSettings().get(BooleanPersistentProperty.SAVE_WORKSPACE_ON_EXIT)) {
            app().persistentSettings().get(StringPersistentProperty.WORKSPACE_PATH).filter(StringUtils::isNotBlank)
                    .map(Paths::get).filter(Files::exists).map(Path::toFile).map(SaveWorkspaceRequest::new)
                    .ifPresent(eventStudio()::broadcast);
        }
    }

}
