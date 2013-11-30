/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.List;
import java.util.Map;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Stage;

import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.pdfsam.configuration.ApplicationContextHolder;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.gui.MainPane;
import org.pdfsam.update.UpdateCheckRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * @author Andrea Vacondio
 * 
 */
public class App extends Application {
    private static final Logger LOG = LoggerFactory.getLogger(App.class);

    @Override
    public void start(Stage primaryStage) {
        AnnotationConfigApplicationContext ctx = ApplicationContextHolder.getContext();
        List<String> styles = (List<String>) ApplicationContextHolder.getContext().getBean("styles");
        Map<String, Image> logos = ApplicationContextHolder.getContext().getBeansOfType(Image.class);
        MainPane mainPane = ctx.getBean(MainPane.class);
        Scene scene = new Scene(mainPane);
        scene.getStylesheets().addAll(styles);
        primaryStage.setScene(scene);
        primaryStage.getIcons().addAll(logos.values());
        primaryStage.setTitle(ApplicationContextHolder.getContext().getBean("appName", String.class));
        primaryStage.show();
        requestCheckForUpdateIfNecessary();
    }

    public static void main(String[] args) {
        LOG.info(DefaultI18nContext.getInstance().i18n("Starting pdfsam"));
        StopWatch stopWatch = new StopWatch();
        stopWatch.start();
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionLogger());
        launch(args);
        stopWatch.stop();
        LOG.info(DefaultI18nContext.getInstance().i18n("Started in {0}",
                DurationFormatUtils.formatDurationWords(stopWatch.getTime(), true, true)));
    }

    private static void requestCheckForUpdateIfNecessary() {
        if (DefaultUserContext.getInstance().isCheckForUpdates()) {
            eventStudio().broadcast(new UpdateCheckRequest());
        }
    }
}
