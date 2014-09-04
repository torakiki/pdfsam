/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/apr/2014
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

import static org.pdfsam.support.RequireUtils.requireNotBlank;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.commons.UrlButton;
import org.pdfsam.update.UpdateAvailableEvent;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.model.exception.InvalidTaskParametersException;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

/**
 * Component dealing with events that require a visual notification
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public class NotificationsController {

    @Inject
    private NotificationsContainer container;

    @PostConstruct
    void init() {
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onAddRequest(AddNotificationRequestEvent event) {
        container.addNotification(event.getTitle(), buildLabel(event.getMessage(), event.getType()));
    }

    private Label buildLabel(String message, NotificationType type) {
        requireNotBlank(message, "Notification text cannot be blank");
        Label textLabel = new Label(message);
        textLabel.getStyleClass().add("notification-text");
        if (type != null) {
            textLabel.getStyleClass().add(type.getStyleClass());
            textLabel.setGraphic(type.getGraphic());
        }
        return textLabel;
    }

    @EventListener
    public void onTaskFailed(TaskExecutionFailedEvent e) {
        if (e.getFailingCause() instanceof InvalidTaskParametersException) {
            container.addNotification(
                    DefaultI18nContext.getInstance().i18n("Invalid parameters"),
                    buildLabel(
                            DefaultI18nContext.getInstance().i18n(
                                    "Input parameters are invalid, open the application messages for details."),
                            NotificationType.ERROR));
        }
    }

    @EventListener
    public void onRemoveRequest(RemoveNotificationRequestEvent event) {
        container.removeNotification(event.getNotificationId());
    }

    @EventListener
    public void onUpdateAvailable(UpdateAvailableEvent event) {
        VBox content = new VBox(3, buildLabel(
                DefaultI18nContext.getInstance().i18n("PDFsam {0} is available for download",
                        event.getAvailableVersion()), NotificationType.INFO), new UrlButton(DefaultI18nContext
                .getInstance().i18n("Download"), "http://www.pdfsam.org/download"));
        content.setAlignment(Pos.TOP_RIGHT);

        container.addStickyNotification(DefaultI18nContext.getInstance().i18n("New version available"), content);
    }
}
