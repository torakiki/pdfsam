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

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;

import org.pdfsam.ui.event.SetActiveDashboardItemRequest;
import org.pdfsam.ui.quickbar.BaseQuickbarButtonsPane;
import org.sejda.eventstudio.annotation.EventListener;

/**
 * Panel containing buttons to access the dashboard items
 * 
 * @author Andrea Vacondio
 *
 */
class QuickbarDashboardButtonsPane extends BaseQuickbarButtonsPane {

    private List<DashboardButton> buttons = new ArrayList<>();

    @Inject
    QuickbarDashboardButtonsPane(List<DashboardItem> items) {
        items.stream().sorted((a, b) -> a.priority() - b.priority()).map(DashboardButton::new)
                .forEach(currentButton -> {
                    currentButton.displayTextProperty().bind(displayTextProperty());
                    getChildren().add(currentButton);
                    buttons.add(currentButton);
                });
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void setCurrentDashboardItem(SetActiveDashboardItemRequest r) {
        buttons.forEach(b -> b.selectIf(r.getActiveItemId()));
    }

}
