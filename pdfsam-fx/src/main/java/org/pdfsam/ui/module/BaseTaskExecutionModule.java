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
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.module.Module;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.parameter.base.TaskParameters;

/**
 * Base class for a {@link Module}
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public abstract class BaseTaskExecutionModule implements Module {

    @Inject
    private ModuleFooterPane footer;

    private BorderPane modulePanel = new BorderPane();

    @PostConstruct
    final void init() {
        footer.runButton().setOnAction(
                event -> eventStudio().broadcast(new TaskExecutionRequestEvent(id(), getParameters())));
        modulePanel.setBottom(footer);
        Pane innerPanel = getInnerPanel();
        innerPanel.getStyleClass().addAll(Style.DEAULT_CONTAINER.css());
        innerPanel.getStyleClass().addAll(Style.CONTAINER.css());
        modulePanel.setCenter(innerPanel);
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
}
