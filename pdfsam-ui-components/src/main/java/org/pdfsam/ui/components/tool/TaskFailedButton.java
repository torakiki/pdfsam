/*
 * This file is part of the PDF Split And Merge source code
 * Created on 29/lug/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.tool;

import javafx.scene.AccessibleRole;
import javafx.scene.control.Button;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.model.ui.ShowLogMessagesRequest;
import org.pdfsam.ui.components.support.Style;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

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
        setText(i18n().tr("Show errors"));
        //TODO set -fx-icon-size to 1.6em
        var icon = FontIcon.of(UniconsLine.TIMES_CIRCLE);
        icon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        setGraphic(icon);
        setMaxHeight(Double.MAX_VALUE);
        setPrefHeight(Double.MAX_VALUE);
        setOnAction(e -> eventStudio().broadcast(new ShowLogMessagesRequest()));
    }
}
