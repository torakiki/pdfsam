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

import java.math.BigDecimal;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.model.exception.TaskOutputVisitException;
import org.sejda.model.notification.event.PercentageOfWorkDoneChangedEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;

/**
 * Horizontal buttons panel shown in the footer
 * 
 * @author Andrea Vacondio
 *
 */
class Footer extends HBox {

    private static final Logger LOG = LoggerFactory.getLogger(Footer.class);

    private ProgressBar bar = new ProgressBar(0);
    private Label statusLabel = new Label();
    private OpenButton open = new OpenButton();
    private TaskFailedButton failed = new TaskFailedButton();

    public Footer(RunButton runButton) {
        this.getStyleClass().addAll("pdfsam-container", "footer-pane");
        this.statusLabel.getStyleClass().add("status-label");
        this.statusLabel.setVisible(false);
        this.bar.setMaxWidth(Double.MAX_VALUE);
        this.bar.getStyleClass().add("pdfsam-footer-bar");
        this.statusLabel.setMaxHeight(Double.MAX_VALUE);
        VBox progressPane = new VBox(statusLabel, bar);
        progressPane.getStyleClass().add("progress-pane");
        VBox.setVgrow(statusLabel, Priority.ALWAYS);
        HBox.setHgrow(bar, Priority.ALWAYS);
        HBox.setHgrow(progressPane, Priority.ALWAYS);
        this.failed.setVisible(false);
        this.open.setVisible(false);
        StackPane buttons = new StackPane(failed, open);
        buttons.setAlignment(Pos.CENTER_LEFT);
        this.getChildren().addAll(runButton, buttons, progressPane);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onTaskExecutionRequest(TaskExecutionRequestEvent event) {
        open.setVisible(false);
        failed.setVisible(false);
        statusLabel.setVisible(true);
        statusLabel.setText(DefaultI18nContext.getInstance().i18n("Requested"));
        bar.setProgress(0);
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
        statusLabel.setText(DefaultI18nContext.getInstance().i18n("Completed"));
        bar.setProgress(1);
    }

    @EventListener
    public void onTaskFailed(TaskExecutionFailedEvent event) {
        open.setVisible(false);
        failed.setVisible(true);
        statusLabel.setText(DefaultI18nContext.getInstance().i18n("Failed"));
    }

    @EventListener
    public void onProgress(PercentageOfWorkDoneChangedEvent event) {
        open.setVisible(false);
        failed.setVisible(false);
        statusLabel.setText(DefaultI18nContext.getInstance().i18n("Running"));
        if (event.isUndetermined()) {
            bar.setProgress(ProgressIndicator.INDETERMINATE_PROGRESS);
        } else {
            bar.setProgress(event.getPercentage().divide(new BigDecimal(100)).doubleValue());
            statusLabel.setText(DefaultI18nContext.getInstance().i18n("Running {0}%",
                    Integer.toString(event.getPercentage().intValue())));
        }
    }
}
