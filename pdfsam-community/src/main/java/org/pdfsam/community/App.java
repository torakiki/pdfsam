/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.community;

import java.io.IOException;

import javafx.application.Preloader;
import javafx.scene.Scene;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

import org.pdfsam.PdfsamApp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.javafx.application.LauncherImpl;

/**
 * PDFsam Community Edition App
 * 
 * @author Andrea Vacondio
 *
 */
public class App extends Preloader {
    private static final Logger LOG = LoggerFactory.getLogger(App.class);
    private Stage stage;
    private CommunityPane pane;

    public static void main(String[] args) {
        LauncherImpl.launchApplication(PdfsamApp.class, App.class, args);
    }

    @Override
    public void start(Stage stage) {
        this.stage = stage;
        this.pane = new CommunityPane();
        stage.initStyle(StageStyle.TRANSPARENT);
        PdfsamCommunityConfig config = new PdfsamCommunityConfig();
        Scene scene = new Scene(pane);
        scene.getStylesheets().add(this.getClass().getResource("/css/community.css").toExternalForm());
        stage.setScene(scene);
        try {
            HBox logoView = new HBox();
            logoView.getChildren().addAll(new ImageView(config.logo32()),
                    new ImageView(this.getClass().getResource("/images/payoff.png").toExternalForm()));
            stage.getIcons().addAll(config.logo16(), config.logo24(), config.logo48(), config.logo64(),
                    config.logo96(), config.logo128(), config.logo256());

            stage.setTitle("PDF Split and Merge Community Edition");
            stage.centerOnScreen();
            stage.show();
        } catch (IOException e) {
            LOG.error("An error occurred during startup", e);
        }

    }

    @Override
    public void handleStateChangeNotification(StateChangeNotification scn) {
        if (scn.getType() == StateChangeNotification.Type.BEFORE_START) {
            stage.hide();
        }
    }

    @Override
    public void handleApplicationNotification(PreloaderNotification info) {
        if (pane != null && info instanceof ProgressNotification) {
            pane.setProgress((ProgressNotification) info);
        }
    }

    @Override
    public void handleProgressNotification(ProgressNotification info) {
        if (pane != null) {
            pane.setProgress(info);
        }
    }
}
