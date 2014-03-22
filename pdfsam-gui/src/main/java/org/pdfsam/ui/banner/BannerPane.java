/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/nov/2013
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
package org.pdfsam.ui.banner;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;

import javafx.fxml.FXMLLoader;
import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.StackPane;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.support.ShowRequestEvent;
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
    @Named("logo35")
    private Image logo;
    @Inject
    private ErrorsNotification errorNotification;

    public BannerPane() {
        getStyleClass().add("pdfsam-banner");
    }

    @PostConstruct
    private void init() throws IOException {
        HBox buttonBar = buildButtonsBar();
        HBox.setHgrow(buttonBar, Priority.ALWAYS);
        HBox logoView = new HBox();
        logoView.getStyleClass().add("pdfsam-logo");
        logoView.getChildren().addAll(new ImageView(logo), payoff);
        getChildren().addAll(logoView, buttonBar);
    }

    private HBox buildButtonsBar() throws IOException {
        // TODO update buttons
        // TODO home button
        HBox buttons = new HBox();
        buttons.getStyleClass().addAll("pdfsam-container", "pdfsam-banner-buttons");
        Button logsButton = new Button();
        logsButton.setGraphic((Group) FXMLLoader.load(new ClassPathResource("/fxml/log-button.fxml").getURL()));
        logsButton.setOnAction(e -> eventStudio().broadcast(new ShowRequestEvent(), "LogStage"));
        logsButton.getStyleClass().add("pdfsam-toolbar-button");
        logsButton.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                "Open the window displaying application messages")));
        StackPane logs = new StackPane(logsButton, errorNotification);
        StackPane.setAlignment(errorNotification, Pos.BOTTOM_LEFT);
        buttons.getChildren().addAll(logs);
        return buttons;
    }

}
