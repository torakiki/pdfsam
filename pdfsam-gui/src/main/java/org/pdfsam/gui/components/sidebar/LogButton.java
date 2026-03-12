/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
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

import jakarta.inject.Inject;
import javafx.scene.shape.Circle;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.gui.components.content.log.ErrorLoggedEvent;
import org.pdfsam.gui.components.content.log.LogContentItem;
import org.pdfsam.model.ui.ShowLogMessagesRequest;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.gui.components.sidebar.SelectableSidebarButton.of;

/**
 * Sidebar button to show the log panel. It also shows a notification dot when there are unread error messages.
 *
 * @author Andrea Vacondio
 */
public class LogButton extends SelectableSidebarButtonWithNotification {
    @Inject
    public LogButton(LogContentItem logItem) {
        super(of(logItem), SidebarButtonWithNotification.notificationOf(new Circle(5)), SidebarNotificationType.ERROR);
        this.getWrapped().setOnAction(e -> eventStudio().broadcast(new ShowLogMessagesRequest()));
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onErrorLoggedEvent(ErrorLoggedEvent event) {
        if (!isSelected()) {
            showNotification();
        }
    }

    @EventListener
    public void onShowErrorMessagesRequest(ShowLogMessagesRequest request) {
        hideNotification();
    }
}
