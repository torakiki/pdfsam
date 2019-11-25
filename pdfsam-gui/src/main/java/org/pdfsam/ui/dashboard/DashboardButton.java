/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
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
package org.pdfsam.ui.dashboard;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.ui.event.SetActiveDashboardItemRequest;
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
        requireNotNullArg(item, "Dashboard item cannot be null");
        this.item = item;
        setGraphic(this.item.graphic());
        setText(this.item.name());
        setOnAction(e -> eventStudio().broadcast(new SetActiveDashboardItemRequest(this.item.id())));
    }

    /**
     * sets the button selected if the id matches
     * 
     * @param id
     */
    void selectIf(String id) {
        setSelected(item.id().equals(id));
    }
}
