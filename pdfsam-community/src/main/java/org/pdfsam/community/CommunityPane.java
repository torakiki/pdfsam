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
import java.io.InputStream;
import java.util.Properties;

import javafx.application.Preloader.ProgressNotification;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Main panel for the community edition
 * 
 * @author Andrea Vacondio
 *
 */
public class CommunityPane extends VBox {
    private static final Logger LOG = LoggerFactory.getLogger(CommunityPane.class);

    private ProgressBar progress;

    CommunityPane() {
        getStyleClass().add("-pdfsam-community-splash");
        HBox logoView = new HBox(15);
        logoView.getChildren().addAll(
                new ImageView(this.getClass().getResource("/images/community/32x32.png").toExternalForm()),
                new ImageView(this.getClass().getResource("/images/payoff.png").toExternalForm()));
        progress = new ProgressBar(0);
        progress.setMaxWidth(Double.MAX_VALUE);
        Label message = new Label("Loading...");
        message.getStyleClass().add("-pdfsam-update-message");
        Properties props = new Properties();
        try (InputStream inputStream = getClass().getClassLoader().getResourceAsStream("pdfsam.properties")) {
            props.load(inputStream);
        } catch (IOException e) {
            LOG.warn("Unable to load version information", e);
        }
        Label version = new Label(String.format("PDFsam Community Edition v.%s",
                props.getProperty("pdfsam.version", "")));
        version.getStyleClass().add("-pdfsam-splash-version");
        version.setMaxWidth(Double.MAX_VALUE);
        getChildren().addAll(logoView, progress, message, version);
    }

    void setProgress(ProgressNotification info) {
        progress.setProgress(info.getProgress());
    }
}
