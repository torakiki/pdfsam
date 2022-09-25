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
package org.pdfsam;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import javafx.application.Application;
import javafx.application.HostServices;
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
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.core.context.DefaultUserContext;
import org.pdfsam.core.context.UserContext;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.injector.Injector;
import org.pdfsam.model.lifecycle.ShutdownEvent;
import org.pdfsam.model.lifecycle.StartupEvent;
import org.pdfsam.model.ui.workspace.SaveWorkspaceRequest;
import org.pdfsam.news.FetchLatestNewsRequest;
import org.pdfsam.service.news.NewsService;
import org.pdfsam.premium.FetchPremiumModulesRequest;
import org.pdfsam.service.Services;
import org.pdfsam.tool.Tool;
import org.pdfsam.ui.MainPane;
import org.pdfsam.ui.SetLatestStageStatusRequest;
import org.pdfsam.ui.StageMode;
import org.pdfsam.service.ui.StageService;
import org.pdfsam.ui.StageStatus;
import org.pdfsam.ui.commons.NativeOpenUrlRequest;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.pdfsam.ui.dashboard.DashboardConfig;
import org.pdfsam.ui.dashboard.preference.PreferenceConfig;
import org.pdfsam.ui.dialog.ConfirmationDialog;
import org.pdfsam.ui.dialog.OpenWithDialog;
import org.pdfsam.ui.dialog.OverwriteConfirmationDialog;
import org.pdfsam.ui.io.SetLatestDirectoryEvent;
import org.pdfsam.ui.log.LogMessageBroadcaster;
import org.pdfsam.ui.log.LoggerConfig;
import org.pdfsam.ui.module.OpenButton;
import org.pdfsam.ui.module.RunButtonTriggerRequest;
import org.pdfsam.ui.notification.NotificationsContainer;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.update.UpdateCheckRequest;
import org.sejda.core.Sejda;
import org.sejda.core.support.io.IOUtils;
import org.sejda.impl.sambox.component.PDDocumentHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.SplashScreen;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.ui.commons.SetActiveModuleRequest.activeteModule;

/**
 * PDFsam application
 * 
 * @author Andrea Vacondio
 * 
 */
public class PdfsamApp extends Application {
    private static final Logger LOG = LoggerFactory.getLogger(PdfsamApp.class);

    private static final StopWatch STOPWATCH = new StopWatch();
    private Stage primaryStage;
    private UserContext userContext = new DefaultUserContext();
    private List<String> rawParameters;
    private boolean clean;
    private Injector injector;
    private LogMessageBroadcaster broadcaster;

    @Override
    public void init() {
        STOPWATCH.start();
        rawParameters = getParameters().getRaw();
        verboseIfRequired();
        startLogAppender();
        System.setProperty(PDDocumentHandler.SAMBOX_USE_ASYNC_WRITER, Boolean.TRUE.toString());
        System.setProperty(Sejda.UNETHICAL_READ_PROPERTY_NAME, Boolean.TRUE.toString());
        System.setProperty(IOUtils.TMP_BUFFER_PREFIX_PROPERTY_NAME, "pdfsam");
        LOG.info("Starting PDFsam");
        clean = rawParameters.contains("--clean") || rawParameters.contains("-clean") || rawParameters.contains("-c");
        cleanUserContextIfNeeded(userContext);
        String localeString = userContext.getLocale();
        if (isNotBlank(localeString)) {
            eventStudio().broadcast(new SetLocaleRequest(localeString));
        }
        String defaultworkingPath = userContext.getDefaultWorkingPath();
        if (isNotBlank(defaultworkingPath)) {
            try {
                if (Files.isDirectory(Paths.get(defaultworkingPath))) {
                    eventStudio().broadcast(new SetLatestDirectoryEvent(new File(defaultworkingPath)));
                }
            } catch (InvalidPathException e) {
                LOG.warn("Unable to set initial directory, default path is invalid.", e);
            }
        }
    }

    private void verboseIfRequired() {
        if (rawParameters.contains("--verbose") || rawParameters.contains("-verbose") || rawParameters.contains("-v")) {
            ((ch.qos.logback.classic.Logger) LoggerFactory.getLogger("org.sejda")).setLevel(Level.DEBUG);
            ((ch.qos.logback.classic.Logger) LoggerFactory.getLogger("org.sejda.sambox")).setLevel(Level.DEBUG);
            ((ch.qos.logback.classic.Logger) LoggerFactory.getLogger("org.pdfsam")).setLevel(Level.TRACE);
            ((ch.qos.logback.classic.Logger) LoggerFactory.getLogger("org.pdfsam.eventstudio")).setLevel(Level.INFO);
            LOG.info("Enabled verbose logging");
        }
    }

    private void cleanUserContextIfNeeded(UserContext userContext) {
        if (clean) {
            userContext.clear();
            LOG.info("Cleared user preferences");
        }
    }

    @Override
    public void start(Stage primaryStage) {
        this.primaryStage = primaryStage;
        injector = initInjector();
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionLogger());
        initSejda();
        cleanIfRequired();
        primaryStage.setScene(initScene());
        primaryStage.getIcons().addAll(injector.instancesOfType(Image.class));
        primaryStage.setTitle(injector.instance(AppBrand.class).name());
        primaryStage.setOnCloseRequest(e -> Platform.exit());
        requestPremiumModulesDescriptionIfRequired();
        initWindowsStatusController(primaryStage);
        initDialogsOwner(primaryStage);
        initActiveModule();
        loadWorkspaceIfRequired();
        initOpenButtons();
        escapeMnemonicsOnFocusLost(primaryStage);
        primaryStage.show();

        requestCheckForUpdateIfRequired();
        requestLatestNewsIfRequired();
        eventStudio().addAnnotatedListeners(this);
        closeSplash();
        STOPWATCH.stop();
        eventStudio().broadcast(new StartupEvent());
        LOG.info(I18nContext.getInstance()
                .i18n("Started in {0}", DurationFormatUtils.formatDurationWords(STOPWATCH.getTime(), true, true)));
        new InputPdfArgumentsController().accept(rawParameters);
    }

    private Injector initInjector() {
        Injector.addConfig(new PdfsamConfig(), new LoggerConfig(), new PreferenceConfig(), new DashboardConfig());
        Services.initServices();
        return Injector.start();
    }

    private void initSejda() {
        AppBrand appBrand = injector.instance(AppBrand.class);
        Sejda.CREATOR = appBrand.shortName() + " v" + appBrand.property(BrandableProperty.VERSION);
    }

    private void startLogAppender() {
        PatternLayoutEncoder encoder = new PatternLayoutEncoder();
        encoder.setPattern("%-5level %nopex [%d{HH:mm:ss}]: %msg%n%xThrowable{50}");
        broadcaster = new LogMessageBroadcaster(encoder);
        broadcaster.start();
    }

    private void closeSplash() {
        Optional.ofNullable(SplashScreen.getSplashScreen()).ifPresent(SplashScreen::close);
    }

    private Scene initScene() {
        MainPane mainPane = injector.instance(MainPane.class);

        NotificationsContainer notifications = injector.instance(NotificationsContainer.class);
        StackPane main = new StackPane();
        StackPane.setAlignment(notifications, Pos.BOTTOM_RIGHT);
        StackPane.setAlignment(mainPane, Pos.TOP_LEFT);
        main.getChildren().addAll(mainPane, notifications);

        StylesConfig styles = injector.instance(StylesConfig.class);

        Scene mainScene = new Scene(main);
        mainScene.getStylesheets().addAll(styles.styles());
        mainScene.getAccelerators().put(new KeyCodeCombination(KeyCode.L, KeyCombination.SHORTCUT_DOWN),
                () -> eventStudio().broadcast(ShowStageRequest.INSTANCE, "LogStage"));
        mainScene.getAccelerators().put(new KeyCodeCombination(KeyCode.Q, KeyCombination.SHORTCUT_DOWN),
                () -> Platform.exit());
        mainScene.getAccelerators().put(new KeyCodeCombination(KeyCode.X, KeyCombination.SHORTCUT_DOWN),
                () -> eventStudio().broadcast(RunButtonTriggerRequest.INSTANCE));
        return mainScene;
    }

    @Override
    public void stop() {
        LOG.info(i18n().tr("Closing PDFsam..."));
        if (nonNull(primaryStage)) {
            StageStatus status = new StageStatus(this.primaryStage.getX(), this.primaryStage.getY(),
                    this.primaryStage.getWidth(), this.primaryStage.getHeight());
            status.setMode(StageMode.valueFor(this.primaryStage));
            eventStudio().broadcast(new SetLatestStageStatusRequest(status));
        }
        saveWorkspaceIfRequired();
        eventStudio().broadcast(new ShutdownEvent());
        injector.close();
    }

    private void requestCheckForUpdateIfRequired() {
        if (injector.instance(UserContext.class).isCheckForUpdates()) {
            eventStudio().broadcast(UpdateCheckRequest.INSTANCE);
        }
    }

    private void cleanIfRequired() {
        if (clean) {
            LOG.debug("Cleaning...");
            injector.instance(NewsService.class).clear();
            injector.instance(StageService.class).clear();
        }
    }

    private void requestLatestNewsIfRequired() {
        if (injector.instance(UserContext.class).isCheckForNews()) {
            eventStudio().broadcast(FetchLatestNewsRequest.INSTANCE);
        }
    }

    /**
     * This is a workaround for https://bugs.openjdk.java.net/browse/JDK-8238731
     * 
     * We fire an ESC key pressed event when the windows looses focus to clear all the mnemonics. Not sure all the edge cases are taken into account, I guess we'll see if some user
     * reports something.
     * 
     * @param primaryStage
     */
    private void escapeMnemonicsOnFocusLost(Stage primaryStage) {
        primaryStage.focusedProperty().addListener((obs, oldVal, newVal) -> {
            if (!newVal) {
                Event.fireEvent(primaryStage.getScene(), new KeyEvent(KeyEvent.KEY_PRESSED, KeyEvent.CHAR_UNDEFINED,
                        KeyCode.ESCAPE.getName(), KeyCode.ESCAPE, false, false, false, false));
            }
        });
    }

    @EventListener
    public void openUrl(NativeOpenUrlRequest event) {
        HostServices services = getHostServices();
        if (nonNull(services)) {
            try {
                services.showDocument(event.getUrl());
            } catch (NullPointerException npe) {
                // service delegate can be null but there's no way to check it first so we have to catch the npe
                LOG.info("Unable to open url using HostServices, trying fallback");
                try {
                    Runtime.getRuntime().exec(getOpenCmd(event.getUrl()));
                } catch (IOException e) {
                    LOG.warn("Unable to open the url", e);
                }
            }
        } else {
            LOG.warn("Unable to open '{}', please copy and paste the url to your browser.", event.getUrl());
        }
    }

    private void initDialogsOwner(Stage primaryStage) {
        injector.instancesOfType(ConfirmationDialog.class).stream().forEach(d -> d.setOwner(primaryStage));
        injector.instancesOfType(OverwriteConfirmationDialog.class).stream().forEach(d -> d.setOwner(primaryStage));
        injector.instance(OpenWithDialog.class).setOwner(primaryStage);
    }

    private void initWindowsStatusController(Stage primaryStage) {
        injector.instance(WindowStatusController.class).setStage(primaryStage);
    }

    private void initActiveModule() {
        String startupModule = userContext.getStartupModule();
        if (isNotBlank(startupModule)) {
            LOG.trace("Activating startup module '{}'", startupModule);
            eventStudio().broadcast(activeteModule(startupModule));
        }
    }

    private void initOpenButtons() {
        List<Tool> tools = injector.instancesOfType(Tool.class);
        List<OpenButton> openButtons = injector.instancesOfType(OpenButton.class);
        for (OpenButton button : openButtons) {
            button.initModules(tools);
        }
    }

    private void requestPremiumModulesDescriptionIfRequired() {
        if (injector.instance(UserContext.class).isFetchPremiumModules()) {
            eventStudio().broadcast(FetchPremiumModulesRequest.INSTANCE);
        }
    }

    private void loadWorkspaceIfRequired() {
        String workspace = ofNullable(getParameters().getNamed().get("workspace")).filter(StringUtils::isNotBlank)
                .orElseGet(userContext::getDefaultWorkspacePath);
        if (isNotBlank(workspace) && Files.exists(Paths.get(workspace))) {
            eventStudio().broadcast(new LoadWorkspaceEvent(new File(workspace)));
        }
    }

    private void saveWorkspaceIfRequired() {
        if (userContext.isSaveWorkspaceOnExit()) {
            String workspace = userContext.getDefaultWorkspacePath();
            if (isNotBlank(workspace) && Files.exists(Paths.get(workspace))) {
                eventStudio().broadcast(new SaveWorkspaceRequest(new File(workspace), true));
            }
        }
    }

    public static String getOpenCmd(String url) throws IOException {
        String os = System.getProperty("os.name").toLowerCase();
        if (os.indexOf("mac") >= 0) {
            return String.format("%s %s", "open", url);
        }
        if (os.indexOf("win") >= 0) {
            return String.format("%s %s", "explorer", url);
        }
        if (os.indexOf("nix") >= 0 || os.indexOf("nux") >= 0 || os.indexOf("aix") > 0) {
            return String.format("%s %s", "xdg-open", url);
        }
        throw new IOException("Unable to identify the OS");
    }
}
