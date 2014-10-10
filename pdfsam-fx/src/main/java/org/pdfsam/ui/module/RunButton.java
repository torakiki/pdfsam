/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/mar/2014
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
package org.pdfsam.ui.module;

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.control.Button;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

/**
 * Run button that enable/disable itself according the task executions events.
 * 
 * @author Andrea Vacondio
 *
 */
class RunButton extends Button {

    public RunButton() {
        getStyleClass().addAll(Style.BUTTON.css());
        setText(DefaultI18nContext.getInstance().i18n("Run"));
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void disableRunButtonIfTaskRequested(TaskExecutionRequestEvent event) {
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
