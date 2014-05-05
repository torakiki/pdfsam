/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mag/2014
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
package org.pdfsam.ui.banner;

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.control.Tooltip;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.event.SetActiveDashboardItemRequest;

import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Button to open the dashboard
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class DashboardButton extends BannerButton {
    @Inject
    @Named("defaultDashboardItemId")
    private String defaultDasboardItem;

    DashboardButton() {
        super(AwesomeIcon.HOME);
        setOnAction(e -> eventStudio().broadcast(new SetActiveDashboardItemRequest(defaultDasboardItem)));
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Open main dashboard")));
    }
}
