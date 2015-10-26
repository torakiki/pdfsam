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

import static org.pdfsam.ui.commons.UrlButton.styledUrlButton;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.ConfigurableProperty;
import org.pdfsam.Pdfsam;
import org.pdfsam.PdfsamEdition;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.UsageService;
import org.pdfsam.update.UpdateAvailableEvent;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.model.exception.InvalidTaskParametersException;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;

/**
 * Component dealing with events that require a visual notification
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public class NotificationsController {

    private static final int TIMES_BEFORE_ENTERPRISE_NOTICE = 5;

    private NotificationsContainer container;
    private UsageService service;
    private Pdfsam pdfsam;

    @Inject
    NotificationsController(NotificationsContainer container, UsageService service, Pdfsam pdfsam) {
        this.container = container;
        this.service = service;
        this.pdfsam = pdfsam;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onAddRequest(AddNotificationRequestEvent event) {
        container.addNotification(event.getTitle(), buildLabel(event.getMessage(), event.getType()));
    }

    private Label buildLabel(String message, NotificationType type) {
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
            container.addNotification(DefaultI18nContext.getInstance().i18n("Invalid parameters"),
                    buildLabel(
                            DefaultI18nContext.getInstance()
                                    .i18n("Input parameters are invalid, open the application messages for details."),
                            NotificationType.ERROR));
        }
    }

    @EventListener
    public void onTaskCompleted(@SuppressWarnings("unused") TaskExecutionCompletedEvent e) {
        long usages = service.getTotalUsage();
        if (PdfsamEdition.COMMUNITY == pdfsam.edition() && (usages % TIMES_BEFORE_ENTERPRISE_NOTICE) == 0) {
            VBox content = new VBox(3,
                    buildLabel(DefaultI18nContext.getInstance()
                            .i18n("You performed {0} tasks with PDFsam, did it help?", Long.toString(usages)),
                    NotificationType.GO_PRO),
                    styledUrlButton(DefaultI18nContext.getInstance().i18n("Give something back"),
                            pdfsam.property(ConfigurableProperty.DONATE_URL), null));
            content.setAlignment(Pos.TOP_RIGHT);

            container.addStickyNotification(DefaultI18nContext.getInstance().i18n("PDFsam worked hard!"), content);
        }
    }

    @EventListener
    public void onRemoveRequest(RemoveNotificationRequestEvent event) {
        container.removeNotification(event.getNotificationId());
    }

    @EventListener
    public void onUpdateAvailable(UpdateAvailableEvent event) {
        VBox content = new VBox(3,
                buildLabel(DefaultI18nContext.getInstance().i18n("PDFsam {0} is available for download",
                        event.getAvailableVersion()), NotificationType.INFO),
                styledUrlButton(DefaultI18nContext.getInstance().i18n("Download"),
                        pdfsam.property(ConfigurableProperty.DOWNLOAD_URL), null));
        content.setAlignment(Pos.TOP_RIGHT);

        container.addStickyNotification(DefaultI18nContext.getInstance().i18n("New version available"), content);
    }
}
