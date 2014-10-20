/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/set/2014
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

import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;

import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.model.exception.TaskOutputVisitException;
import org.sejda.model.notification.event.PercentageOfWorkDoneChangedEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.notification.event.TaskExecutionStartedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;

/**
 * Panel showing task progress information
 * 
 * @author Andrea Vacondio
 *
 */
@Named
@Scope(BeanDefinition.SCOPE_PROTOTYPE)
class ProgressPane extends VBox {
    private static final Logger LOG = LoggerFactory.getLogger(ProgressPane.class);

    private ProgressBar bar = new ProgressBar(0);
    private Label statusLabel = new Label(DefaultI18nContext.getInstance().i18n("Ready"));
    private TaskFailedButton failed = new TaskFailedButton();
    private OpenButton open = new OpenButton();

    public ProgressPane() {
        this.getStyleClass().add("progress-pane");
        this.statusLabel.getStyleClass().add("progress-status");
        this.failed.setVisible(false);
        this.open.setVisible(false);
        this.bar.getStyleClass().add("pdfsam-footer-bar");
        StackPane buttons = new StackPane(failed, open);
        HBox progressStatusBox = new HBox(statusLabel, buttons);
        progressStatusBox.getStyleClass().add("progress-status-pane");
        HBox.setHgrow(statusLabel, Priority.ALWAYS);
        statusLabel.setMaxWidth(Double.MAX_VALUE);
        getChildren().addAll(progressStatusBox, bar);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener(priority = Integer.MIN_VALUE)
    public void onTaskExecutionStarted(TaskExecutionStartedEvent event) {
        statusLabel.setText(DefaultI18nContext.getInstance().i18n("Running"));
    }

    @EventListener
    public void onTaskExecutionRequest(TaskExecutionRequestEvent event) {
        open.setVisible(false);
        failed.setVisible(false);
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
        bar.setProgress(1);
        statusLabel.setText(DefaultI18nContext.getInstance().i18n("Completed"));
    }

    @EventListener
    public void onTaskFailed(TaskExecutionFailedEvent event) {
        open.setVisible(false);
        failed.setVisible(true);
        bar.setProgress(1);
        statusLabel.setText(DefaultI18nContext.getInstance().i18n("Failed"));
    }

    @EventListener
    public void onProgress(PercentageOfWorkDoneChangedEvent event) {
        open.setVisible(false);
        failed.setVisible(false);
        if (event.isUndetermined()) {
            statusLabel.setText(DefaultI18nContext.getInstance().i18n("Running"));
            bar.setProgress(ProgressIndicator.INDETERMINATE_PROGRESS);
        } else {
            statusLabel.setText(String.format("%d %%", event.getPercentage().intValue()));
            bar.setProgress(event.getPercentage().divide(new BigDecimal(100)).doubleValue());
        }
    }

}
