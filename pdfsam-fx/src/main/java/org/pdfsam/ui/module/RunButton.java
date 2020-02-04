/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/mar/2014
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

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.injector.Prototype;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.ui.support.Style;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import de.jensd.fx.glyphs.materialdesignicons.utils.MaterialDesignIconFactory;
import javafx.scene.control.Button;

/**
 * Run button that enable/disable itself according the task executions events.
 * 
 * @author Andrea Vacondio
 *
 */
@Prototype
public class RunButton extends Button {

    public RunButton() {
        getStyleClass().addAll(Style.FOOTER_BUTTON.css());
        setText(DefaultI18nContext.getInstance().i18n("Run"));
        MaterialDesignIconFactory.get().setIcon(this, MaterialDesignIcon.PLAY, "2.0em");
        setMaxHeight(Double.MAX_VALUE);
        setPrefHeight(Double.MAX_VALUE);
        setDefaultButton(true);
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
