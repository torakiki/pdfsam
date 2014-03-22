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
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.awt.Desktop;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import javafx.application.Application;
import javafx.application.HostServices;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.stage.Stage;

import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.pdfsam.configuration.ApplicationContextHolder;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.ui.MainPane;
import org.pdfsam.ui.OpenFileRequestEvent;
import org.pdfsam.ui.OpenUrlRequestEvent;
import org.pdfsam.ui.support.ShowRequestEvent;
import org.pdfsam.update.UpdateCheckRequest;
import org.sejda.eventstudio.Listener;
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
    public void start(Stage primaryStage) {
        STOPWATCH.start();
        LOG.info(DefaultI18nContext.getInstance().i18n("Starting pdfsam"));
        List<String> styles = (List<String>) ApplicationContextHolder.getContext().getBean("styles");
        Map<String, Image> logos = ApplicationContextHolder.getContext().getBeansOfType(Image.class);
        MainPane mainPane = ApplicationContextHolder.getContext().getBean(MainPane.class);
        Scene scene = new Scene(mainPane);
        scene.getStylesheets().addAll(styles);
        primaryStage.setScene(scene);
        primaryStage.getIcons().addAll(logos.values());
        primaryStage.setTitle(ApplicationContextHolder.getContext().getBean("appName", String.class));
        scene.getAccelerators().put(new KeyCodeCombination(KeyCode.L, KeyCombination.SHORTCUT_DOWN),
                () -> eventStudio().broadcast(new ShowRequestEvent(), "LogStage"));
        primaryStage.show();
        eventStudio().add(new TitleController(primaryStage));
        requestCheckForUpdateIfNecessary();
        STOPWATCH.stop();
        eventStudio().addAnnotatedListeners(this);
        LOG.info(DefaultI18nContext.getInstance().i18n("Started in {0}",
                DurationFormatUtils.formatDurationWords(STOPWATCH.getTime(), true, true)));
    }

    public static void main(String[] args) {
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionLogger());
        launch(args);
    }

    private static void requestCheckForUpdateIfNecessary() {
        if (DefaultUserContext.getInstance().isCheckForUpdates()) {
            eventStudio().broadcast(new UpdateCheckRequest());
        }
    }

    @EventListener
    public void openUrl(OpenUrlRequestEvent event) {
        HostServices services = getHostServices();
        if (services != null) {
            services.showDocument(event.getUrl());
        } else {
            LOG.warn("Unable to open '{}', please copy and paste the url to your browser.", event.getUrl());
        }
    }

    @EventListener
    public void openPath(OpenFileRequestEvent event) {
        if (Desktop.isDesktopSupported()) {
            try {
                Desktop.getDesktop().open(event.getFile());
            } catch (IOException e) {
                LOG.error("Unable to open '{}'", event.getFile().getAbsoluteFile());
            }
        }
    }

    public static class TitleController implements Listener<SetTitleEvent> {

        private Stage primaryStage;

        public TitleController(Stage primaryStage) {
            this.primaryStage = primaryStage;
        }

        public void onEvent(SetTitleEvent event) {
            String title = ApplicationContextHolder.getContext().getBean("appName", String.class);
            if (isNotBlank(event.getTitle())) {
                title = String.format("%s - %s", title, event.getTitle());
            }
            primaryStage.setTitle(title);
        }
    }

}
