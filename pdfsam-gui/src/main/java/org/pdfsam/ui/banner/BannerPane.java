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

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.StackPane;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.ui.event.SetTitleEvent;
import org.sejda.eventstudio.annotation.EventListener;

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
    @Inject
    private LogButton logButton;
    @Inject
    private DashboardButton dashboardButton;
    @Inject
    private MenuButton menuButton;
    private Label current = new Label();

    public BannerPane() {
        getStyleClass().add("pdfsam-banner");
        eventStudio().addAnnotatedListeners(this);
    }

    @PostConstruct
    private void init() {
        current.getStyleClass().add("header-title");
        HBox buttonBar = buildButtonsBar();
        HBox.setHgrow(buttonBar, Priority.ALWAYS);
        HBox logoView = new HBox();
        logoView.getStyleClass().add("pdfsam-logo");
        logoView.getChildren().addAll(new ImageView(logo), payoff);
        getChildren().addAll(logoView, current, buttonBar);
    }

    private HBox buildButtonsBar() {
        // TODO update buttons
        HBox buttons = new HBox();
        buttons.getStyleClass().addAll("pdfsam-container", "pdfsam-banner-buttons");
        StackPane logs = new StackPane(logButton, errorNotification);
        StackPane.setAlignment(errorNotification, Pos.BOTTOM_LEFT);
        buttons.getChildren().addAll(logs, dashboardButton, menuButton);
        return buttons;
    }

    @EventListener
    void onChangeTitle(SetTitleEvent event) {
        if (isNotBlank(event.getTitle())) {
            current.setText(String.format("@%s", event.getTitle()));
        } else {
            current.setText("");
        }
    }

}
