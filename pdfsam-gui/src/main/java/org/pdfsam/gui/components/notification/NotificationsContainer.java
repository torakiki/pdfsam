/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/apr/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.util.Duration;

/**
 * Container for the notifications
 * 
 * @author Andrea Vacondio
 *
 */
public class NotificationsContainer extends VBox {

    public NotificationsContainer() {
        getStyleClass().add("notifications");
        setMaxHeight(Region.USE_PREF_SIZE);
    }

    void addNotification(String title, Node message) {
        Platform.runLater(() -> {
            Notification toAdd = doAddNotification(title, message);
            fadeIn(toAdd, e -> toAdd.fadeAway(Duration.seconds(5)));
        });
    }

    void addStickyNotification(String title, Node message) {
        Platform.runLater(() -> {
            Notification toAdd = doAddNotification(title, message);
            fadeIn(toAdd, null);
        });
    }

    private Notification doAddNotification(String title, Node message) {
        Notification toAdd = new Notification(title, message);
        toAdd.onFade(e -> getChildren().remove(toAdd));
        getChildren().add(toAdd);
        return toAdd;
    }

    private void fadeIn(Notification toAdd, EventHandler<ActionEvent> onFinished) {
        FadeTransition transition = new FadeTransition(Duration.millis(300), toAdd);
        transition.setFromValue(0);
        transition.setToValue(1);
        if (onFinished != null) {
            transition.setOnFinished(onFinished);
        }
        transition.play();
    }

    void removeNotification(String id) {
        Node toRemove = lookup(String.format("#%s", id));
        if (toRemove != null && toRemove instanceof Notification) {
            ((Notification) toRemove).fadeAway();
        }
    }

}
