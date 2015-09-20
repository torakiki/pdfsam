/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/apr/2014
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
package org.pdfsam.ui.notification;

import static org.pdfsam.support.RequireUtils.requireNotNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.UUID;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import javafx.animation.FadeTransition;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.StackPane;
import javafx.util.Duration;

/**
 * Container for a basic notification with title, text and icon.
 * 
 * @author Andrea Vacondio
 *
 */
class Notification extends BorderPane {

    private FadeTransition fade = new FadeTransition(Duration.millis(500), this);

    Notification(String title, Node content) {
        requireNotNull(content, "Notification content cannot be blank");
        getStyleClass().add("notification");
        setId(UUID.randomUUID().toString());
        Label titleLabel = new Label(title);
        titleLabel.getStyleClass().add("notification-title");
        Button closeButton = GlyphsDude.createIconButton(FontAwesomeIcon.TIMES);
        closeButton.getStyleClass().addAll("close");
        closeButton.setOnAction(e -> eventStudio().broadcast(new RemoveNotificationRequestEvent(getId())));
        HBox titlePanel = new HBox();
        StackPane buttonPane = new StackPane(closeButton);
        buttonPane.setAlignment(Pos.TOP_RIGHT);
        HBox.setHgrow(buttonPane, Priority.ALWAYS);
        titlePanel.getChildren().addAll(titleLabel, buttonPane);
        BorderPane.setAlignment(content, Pos.CENTER_LEFT);
        setTop(titlePanel);
        setCenter(content);
        setOpacity(0);
        setOnMouseEntered(e -> {
            fade.pause();
            setOpacity(1);
        });
        setOnMouseExited(e -> fade.play());
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
