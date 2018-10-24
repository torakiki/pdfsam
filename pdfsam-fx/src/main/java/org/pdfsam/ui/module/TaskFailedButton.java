/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/lug/2014
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
package org.pdfsam.ui.module;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.pdfsam.ui.support.Style;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.fontawesome.utils.FontAwesomeIconFactory;
import javafx.scene.control.Button;

/**
 * Button requesting to open the log stage to show the user possible errors
 * 
 * @author Andrea Vacondio
 *
 */
class TaskFailedButton extends Button {

    public TaskFailedButton() {
        getStyleClass().addAll(Style.FOOTER_BUTTON.css());
        getStyleClass().add("footer-failed-button");
        setText(DefaultI18nContext.getInstance().i18n("Show errors"));
        FontAwesomeIconFactory.get().setIcon(this, FontAwesomeIcon.TIMES_CIRCLE, "1.6em");
        setMaxHeight(Double.MAX_VALUE);
        setPrefHeight(Double.MAX_VALUE);
        setOnAction(e -> eventStudio().broadcast(ShowStageRequest.INSTANCE, "LogStage"));
    }
}
