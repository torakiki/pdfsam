/*
 * This file is part of the PDF Split And Merge source code
 * Created on 11/apr/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.notification;

import javafx.animation.FadeTransition;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.AccessibleRole;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.util.Duration;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.ui.components.support.Style;

import java.util.UUID;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Container for a basic notification with title, text and icon.
 * 
 * @author Andrea Vacondio
 *
 */
class Notification extends VBox {

    private final FadeTransition fade = new FadeTransition(Duration.millis(500), this);

    Notification(String title, Node content) {
        requireNotNullArg(content, "Notification content cannot be blank");
        getStyleClass().add("notification");
        getStyleClass().addAll(Style.CONTAINER.css());
        setId(UUID.randomUUID().toString());
        setAccessibleRole(AccessibleRole.TEXT);
        setAccessibleText(title);
        setAccessibleRoleDescription(i18n().tr("Notification"));
        var closeButton = new Button("", FontIcon.of(UniconsLine.TIMES));
        closeButton.getStyleClass().addAll("close-button");
        closeButton.setAccessibleText(i18n().tr("Close notification"));
        closeButton.setTooltip(new Tooltip(i18n().tr("Close notification")));
        closeButton.setOnAction(e -> eventStudio().broadcast(new RemoveNotificationRequest(getId())));
        Label titleLabel = new Label(title);
        titleLabel.setPrefWidth(Integer.MAX_VALUE);
        titleLabel.getStyleClass().add("notification-title");
        StackPane top = new StackPane(titleLabel, closeButton);
        top.setAlignment(Pos.TOP_RIGHT);
        getChildren().addAll(top, content);
        setOpacity(0);
        setOnMouseEntered(e -> {
            fade.stop();
            setOpacity(1);
        });
        setOnMouseClicked(e -> {
            setOnMouseEntered(null);
            setOnMouseExited(null);
            fade.stop();
            eventStudio().broadcast(new RemoveNotificationRequest(getId()));
        });
        fade.setFromValue(1);
        fade.setToValue(0);
    }

    void onFade(EventHandler<ActionEvent> onFaded) {
        fade.setOnFinished(onFaded);
    }

    void fadeAway(Duration delay) {
        fade.stop();
        fade.setDelay(delay);
        fade.jumpTo(Duration.ZERO);
        fade.play();
    }

    void fadeAway() {
        fadeAway(Duration.ZERO);
    }

}
