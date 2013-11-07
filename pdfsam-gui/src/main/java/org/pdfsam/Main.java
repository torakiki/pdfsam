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

import java.util.List;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import org.bushe.swing.event.EventServiceExistsException;
import org.bushe.swing.event.EventServiceLocator;
import org.bushe.swing.event.ThreadSafeEventService;
import org.pdfsam.configuration.ApplicationContextHolder;
import org.pdfsam.gui.MainPane;
import org.pdfsam.gui.banner.BannerPane;
import org.pdfsam.gui.menu.AppMenuBar;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * @author Andrea Vacondio
 * 
 */
public class Main extends Application {
    private static final Logger LOG = LoggerFactory.getLogger(Main.class);

    @Override
    public void start(Stage primaryStage) throws EventServiceExistsException {
        EventServiceLocator.setEventService(EventServiceLocator.SERVICE_NAME_EVENT_BUS, new ThreadSafeEventService());
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionLogger());
        AnnotationConfigApplicationContext ctx = ApplicationContextHolder.getContext();
        LOG.debug("here is some message");
        LOG.error("And an error", new IllegalArgumentException("Illegal!!!"));
        List<String> styles = (List<String>) ctx.getBean("styles");

        MainPane mainPane = ctx.getBean(MainPane.class);
        AppMenuBar menuBar = ctx.getBean(AppMenuBar.class);
        BannerPane banner = ctx.getBean(BannerPane.class);
        VBox vbox = new VBox();
        VBox.setVgrow(mainPane, Priority.ALWAYS);

        vbox.getChildren().addAll(menuBar, banner, mainPane);
        Scene scene = new Scene(vbox);
        scene.getStylesheets().addAll(styles);
        primaryStage.setScene(scene);
        primaryStage.show();
    }

    public static void main(String[] args) {
        launch(args);
    }
}
