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
package org.pdfsam.gui.components.workarea;

import javafx.scene.control.Tooltip;
import org.pdfsam.gui.components.quickbar.BaseQuickbarButton;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.model.ui.SetActiveToolRequest;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Quickbar button to select a tool
 *
 * @author Andrea Vacondio
 */
class ToolButton extends BaseQuickbarButton implements ToolBound {

    private final Tool tool;

    ToolButton(Tool tool) {
        requireNotNullArg(tool, "Module cannot be null");
        this.tool = tool;
        setGraphic(this.tool.graphic());
        setText(this.tool.descriptor().name());
        setOnAction(e -> eventStudio().broadcast(new SetActiveToolRequest(tool.id())));
        setTooltip(new Tooltip(this.tool.descriptor().description()));
    }

    @Override
    public String toolBinding() {
        return tool.id();
    }
}
