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

import java.util.function.Consumer;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.apache.commons.lang3.builder.Builder;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.Module;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.ui.notification.AddNotificationRequestEvent;
import org.pdfsam.ui.notification.NotificationType;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.parameter.base.AbstractParameters;

import javafx.application.Platform;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;

/**
 * Base class for a {@link Module}. Modules are automatically scanned for event listener annotations and have their {@link EventStation} set to their {@link #id()}.
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public abstract class BaseTaskExecutionModule implements Module {

    private ModuleFooterPane footer;
    private BorderPane modulePanel = new BorderPane();

    @PostConstruct
    final void init() {
        Pane innerPanel = getInnerPanel();
        innerPanel.getStyleClass().addAll(Style.DEAULT_CONTAINER.css());
        innerPanel.getStyleClass().addAll(Style.MODULE_CONTAINER.css());

        footer.runButton().setOnAction(
                event -> {
                    ErrorTracker errorTracker = new ErrorTracker();
                    Builder<? extends AbstractParameters> builder = getBuilder(errorTracker.andThen(s -> eventStudio()
                            .broadcast(
                                    new AddNotificationRequestEvent(NotificationType.ERROR, s, DefaultI18nContext
                                            .getInstance().i18n("Invalid parameters")))));
                    if (!errorTracker.errorOnBuild) {
                        eventStudio().broadcast(new TaskExecutionRequestEvent(id(), builder.build()));
                    }
                });
        modulePanel.setBottom(footer);
        modulePanel.setCenter(innerPanel);
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
     * @return the inner panel that allows the user to set options and preferences for this module
     */
    protected abstract Pane getInnerPanel();

    /**
     * @param onError
     *            function to be called in case of error while building the task parameters
     * @return a {@link Builder} for the parameters to be used to perform a pdf manipulation
     */
    protected abstract Builder<? extends AbstractParameters> getBuilder(Consumer<String> onError);

    @Inject
    public void setFooter(ModuleFooterPane footer) {
        this.footer = footer;
    }

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

        public void accept(String error) {
            errorOnBuild = true;
        }
    }
}
