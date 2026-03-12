/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.sidebar;

import javafx.beans.property.BooleanProperty;
import javafx.event.ActionEvent;
import javafx.scene.AccessibleRole;
import javafx.scene.Node;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * A component that has sidebutton and a notification to call for the user attention. It can be used as a button in the sidebar.
 *
 * @author Andrea Vacondio
 */
class SidebarButtonWithNotification<T extends SidebarButton> extends StackPane {

    private final T wrapped;
    private final Node graphic;
    private final SidebarNotificationType notificationType;

    public SidebarButtonWithNotification(T wrapped, Node notificationGraphic,
            SidebarNotificationType notificationType) {
        requireNotNullArg(wrapped, "Wrapped button cannot be null");
        requireNotNullArg(wrapped.getGraphic(), "Wrapped button graphic cannot be null");
        requireNotNullArg(notificationType, "Notification type cannot be null");
        this.wrapped = wrapped;
        this.graphic = notificationGraphic;
        this.notificationType = notificationType;
        this.setFocusTraversable(false);
        this.setAccessibleRole(AccessibleRole.BUTTON);
        this.setAccessibleText(wrapped.getAccessibleText());
        notificationGraphic.setVisible(false);
        setStyle("-fx-background-color: rgba(0, 0, 0, 0);");
        var notification = new Pane();
        notification.getStyleClass().addAll("sidebar-button-notification-container", notificationType.getCssClass());
        notification.getChildren().add(notificationGraphic);
        notification.setMouseTransparent(true);
        notification.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        wrapped.addEventHandler(ActionEvent.ACTION, e -> this.graphic.setVisible(false));
        this.getChildren().addAll(wrapped, notification);
        //keep the notification indicator in the correct place
        this.wrapped.getGraphic().boundsInParentProperty().addListener(
                (observable, oldValue, newValue) -> this.graphic.relocate(newValue.getMaxX() - 10,
                        newValue.getMaxY() - 10));
        this.graphic.visibleProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue) {
                this.wrapped.getStyleClass().add(notificationType.getCssClass());
                this.wrapped.setAccessibleText(i18n().tr("{0} (with notification)", this.wrapped.getText()));
            } else {
                this.wrapped.getStyleClass().remove(notificationType.getCssClass());
                this.wrapped.setAccessibleText(this.wrapped.getText());
            }
        });
    }

    public void showNotification() {
        this.graphic.setVisible(true);
    }

    public void hideNotification() {
        this.graphic.setVisible(false);
    }

    public BooleanProperty displayTextProperty() {
        return this.wrapped.displayTextProperty();
    }

    T getWrapped() {
        return wrapped;
    }

    /**
     * Makes a notification node out of the input node
     */
    public static Node notificationOf(Node node) {
        node.getStyleClass().add("notification");
        node.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        node.setAccessibleText("");
        return node;
    }
}
