/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
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
package org.pdfsam.ui.dashboard;

import static org.pdfsam.support.RequireUtils.require;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.ui.quickbar.BaseQuickbarButton;

/**
 * Button for the quickbar of the dashboard
 * 
 * @author Andrea Vacondio
 *
 */
class DashboardButton extends BaseQuickbarButton {
    private DashboardItem item;

    DashboardButton(DashboardItem item) {
        require(item != null, "Dashboard item cannot be null");
        this.item = item;
        setGraphic(this.item.graphic());
        setText(this.item.name());
        setOnAction(e -> eventStudio().broadcast(new SetCurrentDashboardItem(this.item.id())));
    }

    String itemId() {
        return item.id();
    }
}
