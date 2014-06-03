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
import javafx.geometry.Side;
import javafx.scene.control.Tooltip;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.event.ShowStageRequest;

import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Button to open the menu
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class MenuButton extends BannerButton {
    @Inject
    private AppContextMenu menu;

    MenuButton() {
        super(AwesomeIcon.BARS);
        setOnAction(e -> eventStudio().broadcast(new ShowStageRequest(), "LogStage"));
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Open menu")));

    }

    @PostConstruct
    private void initMenues() {
        setOnAction((e) -> menu.show(this, Side.BOTTOM, 0, 0));
    }
}
