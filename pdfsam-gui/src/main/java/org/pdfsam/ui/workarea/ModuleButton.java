/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/feb/2014
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
package org.pdfsam.ui.workarea;

import static org.pdfsam.ui.commons.SetActiveModuleRequest.activeteModule;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.tool.Tool;
import org.pdfsam.ui.quickbar.BaseQuickbarButton;

import javafx.scene.control.Tooltip;

/**
 * Quickbar button to select a module
 * 
 * @author Andrea Vacondio
 *
 */
class ModuleButton extends BaseQuickbarButton {

    private Tool tool;

    ModuleButton(Tool tool) {
        requireNotNullArg(tool, "Module cannot be null");
        this.tool = tool;
        setGraphic(this.tool.graphic());
        setText(this.tool.descriptor().getName());
        setOnAction(e -> eventStudio().broadcast(activeteModule(ModuleButton.this.tool.id())));
        setTooltip(new Tooltip(this.tool.descriptor().getDescription()));
    }

    String moduleId() {
        return tool.id();
    }
}
