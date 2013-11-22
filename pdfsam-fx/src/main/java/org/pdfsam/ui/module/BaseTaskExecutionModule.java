/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.module;

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.Module;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.pdf.PdfLoadCompletedEvent;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.parameter.base.TaskParameters;

/**
 * Abstract implementation of a pdfsam module providing common features to every module whose purpose is to execute a pdf manipulation task.
 * 
 * @author Andrea Vacondio
 * 
 */
public abstract class BaseTaskExecutionModule implements Module {

    private Button runButton = new Button();
    private BorderPane modulePanel = new BorderPane();

    public BaseTaskExecutionModule() {
        init();
        eventStudio().addAnnotatedListeners(new RunButtonStatusController());
    }

    private void init() {
        runButton.getStyleClass().addAll(Style.BUTTON.css());
        runButton.setText(DefaultI18nContext.getInstance().i18n("Run"));
        runButton.setOnAction(event -> eventStudio().broadcast(new TaskExecutionRequestEvent(id(), getParameters())));
        // TODO set the run button graphic
        // runButton.setGraphic(RunAction.class.getResource("/images/run.png"));
        modulePanel.getStyleClass().addAll(Style.CONTAINER.css());
        HBox buttonBar = new HBox();
        buttonBar.getStyleClass().addAll(Style.CONTAINER.css());
        buttonBar.setAlignment(Pos.CENTER_RIGHT);
        buttonBar.getChildren().add(runButton);
        modulePanel.setBottom(buttonBar);
        ScrollPane scrollPane = new ScrollPane();
        scrollPane.setContent(getInnerPanel());
        modulePanel.setCenter(scrollPane);
    }

    @EventListener
    public void disableRunButtonWhileLoadingDocuments(PdfLoadRequestEvent event) {
        // still loading
        runButton.setDisable(true);
    }

    @EventListener
    public void enableRunButtonOnLoadDocumentsCompletion(PdfLoadCompletedEvent event) {
        // I'm done loading documents
        runButton.setDisable(false);
    }

    @EventStation
    public abstract String id();

    /**
     * @return the inner panel that allows the user to set options and preferences for this module
     */
    protected abstract Pane getInnerPanel();

    /**
     * @return parameters to be used to perform a pdf manipulation
     */
    protected abstract TaskParameters getParameters();

    public Pane modulePanel() {
        return modulePanel;
    }

    /**
     * Handles the run button status by listening to events affecting its status and changing it accordingly.
     * 
     * @author Andrea Vacondio
     * 
     */
    final class RunButtonStatusController {

        // we give highest priority to be sure this is executed before the actual task.
        // we first want to disable the button and then execute the task
        @EventListener(priority = Integer.MIN_VALUE)
        public void disableRunButtonIfTaskRequested(TaskExecutionRequestEvent event) {
            runButton.setDisable(true);
        }

        @EventListener
        public void enableRunButtonOnTaskCompletion(TaskExecutionCompletedEvent event) {
            runButton.setDisable(false);
        }

        @EventListener
        public void enableRunButtonOnTaskFailure(TaskExecutionFailedEvent event) {
            runButton.setDisable(false);
        }

    }

}
