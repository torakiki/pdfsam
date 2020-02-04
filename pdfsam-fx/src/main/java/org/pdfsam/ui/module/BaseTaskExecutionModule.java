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

import java.util.function.Consumer;

import org.apache.commons.lang3.builder.Builder;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.Module;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.ui.notification.AddNotificationRequestEvent;
import org.pdfsam.ui.notification.NotificationType;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.sejda.model.parameter.base.AbstractParameters;

import javafx.application.Platform;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

/**
 * Base class for a {@link Module}. Modules are automatically scanned for event listener annotations and have their {@link EventStation} set to their {@link #id()}.
 * 
 * @author Andrea Vacondio
 *
 */
public abstract class BaseTaskExecutionModule implements Module {

    private BorderPane modulePanel = new BorderPane();
    private Footer footer;

    public BaseTaskExecutionModule(Footer footer) {
        this.footer = footer;
    }

    protected final void initModuleSettingsPanel(VBox panel) {
        panel.getStyleClass().addAll(Style.DEAULT_CONTAINER.css());
        panel.getStyleClass().addAll(Style.MODULE_CONTAINER.css());
        panel.getChildren().add(footer);

        footer.runButton().setOnAction(event -> {
            ErrorTracker errorTracker = new ErrorTracker();
            Builder<? extends AbstractParameters> builder = getBuilder(errorTracker
                    .andThen(s -> eventStudio().broadcast(new AddNotificationRequestEvent(NotificationType.ERROR, s,
                            DefaultI18nContext.getInstance().i18n("Invalid parameters")))));
            if (!errorTracker.errorOnBuild) {
                eventStudio().broadcast(new TaskExecutionRequestEvent(id(), builder.build()));
            }
        });
        modulePanel.setCenter(panel);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public final void saveStateData(SaveWorkspaceEvent event) {
        onSaveWorkspace(event.getDataForModule(id()));
    }

    @EventListener
    public final void restoreState(LoadWorkspaceEvent event) {
        Platform.runLater(() -> onLoadWorkspace(event.getData(id())));
    }

    /**
     * @param onError
     *            function to be called in case of error while building the task parameters
     * @return a {@link Builder} for the parameters to be used to perform a pdf manipulation
     */
    protected abstract Builder<? extends AbstractParameters> getBuilder(Consumer<String> onError);

    @Override
    public Pane modulePanel() {
        return modulePanel;
    }

    /**
     * It keeps track of errors during the build step and allow for a later assessment of the build process.
     * 
     * @author Andrea Vacondio
     *
     */
    private static class ErrorTracker implements Consumer<String> {
        boolean errorOnBuild = false;

        @Override
        public void accept(String error) {
            errorOnBuild = true;
        }
    }
}
