/*
 * This file is part of the PDF Split And Merge source code
 * Created on 21/mar/2014
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
import javafx.scene.control.Tooltip;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Prototype;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Run button that enable/disable itself according the task executions events.
 *
 * @author Andrea Vacondio
 */
@Prototype
public class RunButton extends Button {

    public RunButton() {
        getStyleClass().addAll(Style.RUN_BUTTON.css());
        setText(i18n().tr("Run"));
        setTooltip(new Tooltip(
                i18n().tr("Run the task") + " (" + RunButtonTriggerRequest.KEY_CODE_COMBINATION.getDisplayText()
                        + ")"));
        //TODO set -fx-icon-size to 2.0em
        var icon = FontIcon.of(UniconsLine.PLAY);
        icon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        setGraphic(icon);
        setMaxHeight(Double.MAX_VALUE);
        setPrefHeight(Double.MAX_VALUE);
        setDefaultButton(true);
        setAccessibleText(i18n().tr("Run the task"));
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void disableRunButtonIfTaskRequested(TaskExecutionRequest event) {
        setDisable(true);
    }

    @EventListener
    public void enableRunButtonOnTaskCompletion(TaskExecutionCompletedEvent event) {
        setDisable(false);
    }

    @EventListener
    public void enableRunButtonOnTaskFailure(TaskExecutionFailedEvent event) {
        setDisable(false);
    }
}
