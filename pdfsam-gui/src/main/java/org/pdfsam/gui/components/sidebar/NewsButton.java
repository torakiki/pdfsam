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
import javafx.application.Platform;
import javafx.scene.control.Tooltip;
import javafx.scene.shape.Circle;
import org.kordamp.ikonli.boxicons.BoxiconsRegular;
import org.kordamp.ikonli.javafx.FontIcon;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.model.news.LatestNewsResponse;
import org.pdfsam.model.news.ToggleNewsPanelRequest;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Sidebar button to toggle the news panel. It also shows a notification dot when there are unread news.
 *
 * @author Andrea Vacondio
 */
public class NewsButton extends SidebarButtonWithNotification<SidebarButton> {
    @Inject
    public NewsButton() {
        super(new SidebarButton(i18n().tr("News"), new FontIcon(BoxiconsRegular.NEWS)),
                SidebarButtonWithNotification.notificationOf(new Circle(5)), SidebarNotificationType.INFO);
        getWrapped().setDisable(true);
        getWrapped().setAccessibleText(i18n().tr("What's new"));
        getWrapped().setTooltip(new Tooltip(i18n().tr("What's new")));
        getWrapped().setOnAction(e -> eventStudio().broadcast(ToggleNewsPanelRequest.INSTANCE));
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onLatestNews(LatestNewsResponse event) {
        if (!event.latestNews().isEmpty()) {
            Platform.runLater(() -> {
                getWrapped().setDisable(false);
                if (!event.isUpToDate()) {
                    showNotification();
                    getWrapped().setAccessibleText(i18n().tr("There are news available"));
                }
            });
        }
    }

}
