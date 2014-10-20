/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.ui.event.SetActiveModuleRequest.activeteModule;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.Map;

import javafx.application.Application;
import javafx.application.HostServices;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.pdfsam.configuration.ApplicationContextHolder;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.SetLocaleEvent;
import org.pdfsam.context.UserContext;
import org.pdfsam.ui.MainPane;
import org.pdfsam.ui.StylesConfig;
import org.pdfsam.ui.commons.OpenUrlRequest;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.pdfsam.ui.dialog.OverwriteConfirmationDialog;
import org.pdfsam.ui.io.SetLatestDirectoryEvent;
import org.pdfsam.ui.notification.NotificationsContainer;
import org.pdfsam.update.UpdateCheckRequest;
import org.sejda.core.Sejda;
import org.sejda.eventstudio.EventStudio;
import org.sejda.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Andrea Vacondio
 * 
 */
public class App extends Application {
    private static final Logger LOG = LoggerFactory.getLogger(App.class);
    private static StopWatch STOPWATCH = new StopWatch();

    @Override
    public void init() {
        STOPWATCH.start();
        LOG.info("Starting PDFsam");
        System.setProperty(Sejda.UNETHICAL_READ_PROPERTY_NAME, "true");
        System.setProperty(EventStudio.MAX_QUEUE_SIZE_PROP, "20");
        UserContext userContext = new DefaultUserContext();
        String localeString = userContext.getLocale();
        if (isNotBlank(localeString)) {
            eventStudio().broadcast(new SetLocaleEvent(localeString));
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

    @Override
    public void start(Stage primaryStage) {
        StylesConfig styles = ApplicationContextHolder.getContext().getBean(StylesConfig.class);
        Map<String, Image> logos = ApplicationContextHolder.getContext().getBeansOfType(Image.class);
        MainPane mainPane = ApplicationContextHolder.getContext().getBean(MainPane.class);

        NotificationsContainer notifications = ApplicationContextHolder.getContext().getBean(
                NotificationsContainer.class);
        StackPane main = new StackPane();
        StackPane.setAlignment(notifications, Pos.BOTTOM_RIGHT);
        StackPane.setAlignment(mainPane, Pos.TOP_LEFT);
        main.getChildren().addAll(mainPane, notifications);

        Scene scene = new Scene(main);
        scene.getStylesheets().addAll(styles.styles());
        primaryStage.setScene(scene);
        primaryStage.getIcons().addAll(logos.values());
        primaryStage.setTitle(ApplicationContextHolder.getContext().getBean(Pdfsam.class).name());
        scene.getAccelerators().put(new KeyCodeCombination(KeyCode.L, KeyCombination.SHORTCUT_DOWN),
                () -> eventStudio().broadcast(new ShowStageRequest(), "LogStage"));
        initWindowsStatusController(primaryStage);
        initOverwriteDialogController(primaryStage);
        initActiveModule();
        primaryStage.show();
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionLogger());
        requestCheckForUpdateIfNecessary();
        eventStudio().addAnnotatedListeners(this);
        STOPWATCH.stop();
        LOG.info(DefaultI18nContext.getInstance().i18n("Started in {0}",
                DurationFormatUtils.formatDurationWords(STOPWATCH.getTime(), true, true)));
    }

    @Override
    public void stop() {
        LOG.info(DefaultI18nContext.getInstance().i18n("Closing PDFsam..."));
        ApplicationContextHolder.getContext().close();
    }

    public static void main(String[] args) {
        launch(args);
    }

    private static void requestCheckForUpdateIfNecessary() {
        if (ApplicationContextHolder.getContext().getBean(UserContext.class).isCheckForUpdates()) {
            eventStudio().broadcast(new UpdateCheckRequest());
        }
    }

    @EventListener
    public void openUrl(OpenUrlRequest event) {
        HostServices services = getHostServices();
        if (services != null) {
            services.showDocument(event.getUrl());
        } else {
            LOG.warn("Unable to open '{}', please copy and paste the url to your browser.", event.getUrl());
        }
    }

    private void initOverwriteDialogController(Stage primaryStage) {
        OverwriteConfirmationDialog overwriteDialog = ApplicationContextHolder.getContext().getBean(
                OverwriteConfirmationDialog.class);
        overwriteDialog.setOwner(primaryStage);
    }

    private void initWindowsStatusController(Stage primaryStage) {
        WindowStatusController stageStatusController = ApplicationContextHolder.getContext().getBean(
                WindowStatusController.class);
        stageStatusController.setStage(primaryStage);
    }

    private void initActiveModule() {
        String startupModule = new DefaultUserContext().getStartupModule();
        if (isNotBlank(startupModule)) {
            LOG.trace("Activating startup module '{}'", startupModule);
            eventStudio().broadcast(activeteModule(startupModule));
        }
    }
}
