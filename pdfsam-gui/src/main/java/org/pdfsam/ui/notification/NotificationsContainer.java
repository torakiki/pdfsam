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

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.animation.FadeTransition;
import javafx.scene.Node;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.util.Duration;

import javax.annotation.PostConstruct;
import javax.inject.Named;

import org.sejda.eventstudio.annotation.EventListener;

/**
 * Container for the notifications
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public class NotificationsContainer extends VBox {

    public NotificationsContainer() {
        getStyleClass().add("notifications");
        setMaxHeight(Region.USE_PREF_SIZE);
    }

    @PostConstruct
    void init() {
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onAddRequest(AddNotificationRequestEvent event) {
        Notification toAdd = new Notification(event.getTitle(), event.getMessage(), event.getType());
        getChildren().add(toAdd);
        toAdd.fadeAway(e -> getChildren().remove(toAdd), Duration.millis(2000));
        FadeTransition transition = new FadeTransition(Duration.millis(300), toAdd);
        transition.setFromValue(0);
        transition.setToValue(1);
        transition.play();
    }

    @EventListener
    public void onRemoveRequest(RemoveNotificationRequestEvent event) {
        removeNotification(event.getNotificationId());
    }

    private void removeNotification(String id) {
        Node toRemove = getChildById(id);
        if (toRemove != null && toRemove instanceof Notification) {
            ((Notification) toRemove).fadeAway();
        }
    }

    private Node getChildById(String id) {
        for (Node current : getChildren()) {
            if (id.equals(current.getId())) {
                return current;
            }
        }
        return null;
    }
}
