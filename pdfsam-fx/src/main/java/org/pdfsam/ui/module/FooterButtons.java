/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18 gen 2016
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

import static java.util.Objects.isNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.module.TaskExecutionRequestEvent;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.model.exception.TaskOutputVisitException;
import org.sejda.model.notification.event.PercentageOfWorkDoneChangedEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;

/**
 * @author Andrea Vacondio
 *
 */
class FooterButtons extends HBox {

    private static final Logger LOG = LoggerFactory.getLogger(FooterButtons.class);

    private Label progressLabel = new Label("0%");
    private OpenButton open = new OpenButton();
    private TaskFailedButton failed = new TaskFailedButton();

    public FooterButtons(RunButton runButton) {
        this.getStyleClass().add("footer-buttons-pane");
        this.progressLabel.getStyleClass().add("progress-label");
        this.failed.setVisible(false);
        this.open.setVisible(false);
        StackPane buttons = new StackPane(failed, open);
        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);
        this.getChildren().addAll(progressLabel, spacer, buttons, runButton);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onTaskExecutionRequest(TaskExecutionRequestEvent event) {
        open.setVisible(false);
        failed.setVisible(false);
        progressLabel.setText("0%");
        try {
            if (!isNull(event.getParameters().getOutput())) {
                event.getParameters().getOutput().accept(open);
            }
        } catch (TaskOutputVisitException e) {
            LOG.warn("This should never happen", e);
        }
    }

    @EventListener
    public void onTaskCompleted(TaskExecutionCompletedEvent event) {
        open.setVisible(true);
        failed.setVisible(false);
        progressLabel.setText("100%");
    }

    @EventListener
    public void onTaskFailed(TaskExecutionFailedEvent event) {
        open.setVisible(false);
        failed.setVisible(true);
    }

    @EventListener
    public void onProgress(PercentageOfWorkDoneChangedEvent event) {
        open.setVisible(false);
        failed.setVisible(false);
        if (event.isUndetermined()) {
            // TODO
        } else {
            progressLabel.setText(String.format("%d %%", event.getPercentage().intValue()));
        }
    }
}
