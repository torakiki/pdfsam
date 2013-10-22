/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ott/2013
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
package org.pdfsam.gui.about;

import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.apache.commons.io.FileUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.UrlButton;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.env.Environment;

/**
 * Panel displaying PDFsam logo and About panel informations
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class AboutPane extends BorderPane {

    @Inject
    @Qualifier("logo")
    private Group logo;
    @Inject
    private Environment env;

    public AboutPane() {
        getStyleClass().add("pdfsam-container");
    }

    @PostConstruct
    void init() {
        VBox labels = new VBox();
        labels.setAlignment(Pos.CENTER);
        labels.getStyleClass().add("pdfsam-container");
        labels.setId("aboutLabelsContainer");
        Label appName = new Label(String.format("PDF Split and Merge %s", env.getProperty("pdfsam.package")));
        appName.setId("aboutLabelAppName");
        labels.getChildren().add(appName);
        Label versionLabel = new Label(String.format("ver. %s", env.getProperty("pdfsam.version")));
        versionLabel.setId("versionLabel");
        labels.getChildren().add(versionLabel);

        labels.getChildren().add(new Label("Copyright 2006-2013 by Andrea Vacondio"));
        labels.getChildren().add(
                new Label(String.format("%s %s", System.getProperty("java.runtime.name"),
                        System.getProperty("java.runtime.version"))));
        labels.getChildren().add(
                new Label(DefaultI18nContext.getInstance().i18n("Max memory {0}",
                        FileUtils.byteCountToDisplaySize(Runtime.getRuntime().maxMemory()))));
        HBox hbox = new HBox();
        hbox.setAlignment(Pos.CENTER);
        hbox.getStyleClass().addAll("pdfsam-container", "button-container");
        hbox.getChildren().addAll(
                new UrlButton("Home", "http://www.pdfsam.org"),
                new UrlButton(DefaultI18nContext.getInstance().i18n("License"),
                        "http://www.gnu.org/licenses/gpl-3.0.html"),
                new UrlButton("Twitter", "https://twitter.com/PDFsamOSS"),
                new UrlButton("GitHub", "https://github.com/torakiki/pdfsam"));
        labels.getChildren().add(hbox);
        VBox logoPane = new VBox();
        logoPane.setAlignment(Pos.CENTER);
        logoPane.setId("logoContainer");
        logoPane.getChildren().add(logo);
        setLeft(logoPane);
        setCenter(labels);
    }
}
