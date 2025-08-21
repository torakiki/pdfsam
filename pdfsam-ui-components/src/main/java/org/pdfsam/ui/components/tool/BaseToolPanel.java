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

import javafx.application.Platform;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import org.apache.commons.lang3.builder.Builder;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.model.ui.workspace.LoadWorkspaceResponse;
import org.pdfsam.model.ui.workspace.SaveWorkspaceRequest;
import org.pdfsam.model.ui.workspace.WorkspaceData;
import org.pdfsam.ui.components.notification.AddNotificationRequest;
import org.pdfsam.ui.components.notification.NotificationType;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.parameter.base.AbstractParameters;

import java.util.Map;
import java.util.function.Consumer;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * Base class for a {@link Tool}. Tools are automatically scanned for event listener annotations and have their {@link EventStation} set to their {@link #toolBinding()}.
 *
 * @author Andrea Vacondio
 */
public abstract class BaseToolPanel extends BorderPane implements ToolBound {

    private final Footer footer;
    private final String toolId;

    public BaseToolPanel(String toolId, Footer footer) {
        requireNotBlank(toolId, "Bound tool id cannot be blank");
        this.toolId = toolId;
        this.footer = footer;
    }

    protected final void initModuleSettingsPanel(VBox panel) {
        panel.getStyleClass().addAll(Style.DEAULT_CONTAINER.css());
        panel.getStyleClass().addAll(Style.MODULE_CONTAINER.css());
        panel.getChildren().add(footer);

        footer.runButton().setOnAction(event -> {
            ErrorTracker errorTracker = new ErrorTracker();
            Builder<? extends AbstractParameters> builder = getBuilder(errorTracker.andThen(
                    s -> eventStudio().broadcast(
                            new AddNotificationRequest(NotificationType.ERROR, s, i18n().tr("Invalid parameters")))));
            if (!errorTracker.errorOnBuild) {
                eventStudio().broadcast(new TaskExecutionRequest(toolBinding(), builder.build()));
            }
        });
        setCenter(panel);
        eventStudio().addAnnotatedListeners(this);
    }

    @Override
    public String toolBinding() {
        return toolId;
    }
    
    @EventListener
    public final void saveStateData(SaveWorkspaceRequest event) {
        onSaveWorkspace(event.getData(toolBinding()));
    }

    @EventListener
    public final void restoreState(LoadWorkspaceResponse event) {
        Platform.runLater(() -> onLoadWorkspace(event.data()));
    }

    /**
     * Request to add the tool state to the given data map in a ChainOfResponsibility fashion.
     *
     * @param data
     */
    public abstract void onSaveWorkspace(Map<String, String> data);

    /**
     * Request to restore the module state using the provided data.
     *
     * @param workspace
     */
    public abstract void onLoadWorkspace(WorkspaceData workspace);

    @EventListener
    public void onRunButtonAccelerator(RunButtonTriggerRequest request) {
        footer.runButton().fire();
    }

    /**
     * @param onError function to be called in case of error while building the task parameters
     * @return a {@link Builder} for the parameters to be used to perform a pdf manipulation
     */
    protected abstract Builder<? extends AbstractParameters> getBuilder(Consumer<String> onError);

    /**
     * It keeps track of errors during the build step and allow for a later assessment of the build process.
     *
     * @author Andrea Vacondio
     */
    private static class ErrorTracker implements Consumer<String> {
        boolean errorOnBuild = false;

        @Override
        public void accept(String error) {
            errorOnBuild = true;
        }
    }
}
