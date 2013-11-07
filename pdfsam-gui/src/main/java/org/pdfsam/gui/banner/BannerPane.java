/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/nov/2013
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
package org.pdfsam.gui.banner;

import java.io.IOException;

import javafx.fxml.FXMLLoader;
import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.control.Button;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.gui.log.LogStage;
import org.pdfsam.ui.ShowStageHandler;
import org.pdfsam.ui.support.Style;
import org.springframework.core.io.ClassPathResource;

/**
 * Panel showed on the top part of the application. It displays branding images and a toolbar
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class BannerPane extends HBox {

    @Inject
    private ImageView payoff;
    @Inject
    @Named("logo50")
    private Image logo;
    @Inject
    private LogStage logStage;
    @Inject
    private ErrorsNotification errorNotification;

    public BannerPane() {
        getStyleClass().addAll(Style.BANNER.css());
    }

    @PostConstruct
    private void init() throws IOException {
        HBox buttonBar = buildButtonsBar();
        HBox.setHgrow(buttonBar, Priority.ALWAYS);
        Region spacer = new Region();
        spacer.getStyleClass().addAll(Style.BANNER_SPACER.css());
        HBox logoView = new HBox();
        logoView.getStyleClass().addAll(Style.BANNER_LOGO.css());
        logoView.getChildren().addAll(new ImageView(logo), payoff);
        getChildren().addAll(logoView, spacer, buttonBar);
    }

    private HBox buildButtonsBar() throws IOException {
        // TODO real icons
        // TODO update buttons
        // TODO home button
        HBox buttons = new HBox();
        buttons.getStyleClass().addAll(Style.BANNER_BUTTONS.css());
        Button logsButton = new Button();
        logsButton.setGraphic((Group) FXMLLoader.load(new ClassPathResource("/fxml/LogViewer.fxml").getURL()));
        logsButton.setOnAction(new ShowStageHandler(logStage));
        logsButton.getStyleClass().addAll(Style.TOOLBAR_BUTTON.css());
        StackPane logs = new StackPane(logsButton, errorNotification);
        StackPane.setAlignment(errorNotification, Pos.TOP_LEFT);
        buttons.getChildren().add(logs);
        return buttons;
    }

}
