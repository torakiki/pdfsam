/*
 * This file is part of the PDF Split And Merge source code
 * Created on 06/02/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.sidebar;

import javafx.scene.control.Tooltip;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolCategory;
import org.pdfsam.model.ui.SetActiveContentItemRequest;

import static java.util.Optional.ofNullable;
import static java.util.function.Predicate.not;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * @author Andrea Vacondio
 */
public class ToolSelectableSidebarButton extends SelectableSidebarButton {

    private final int order;
    private final ToolCategory category;

    ToolSelectableSidebarButton(String id, String text, ToolCategory category, int order) {
        super(id, text);
        this.order = order;
        this.category = category;
        this.getStyleClass().add("tool-selectable-sidebar-button");
    }

    public int order() {
        return order;
    }

    public ToolCategory category() {
        return category;
    }

    /**
     * Factory method for a {@link ToolSelectableSidebarButton} created from a {@link Tool}
     */
    public static ToolSelectableSidebarButton of(Tool tool, int order) {
        requireNotNullArg(tool, "Tool cannot be null");
        var button = new ToolSelectableSidebarButton(tool.id(), tool.descriptor().name(), tool.descriptor().category(),
                order);
        button.setOnAction(e -> eventStudio().broadcast(new SetActiveContentItemRequest(tool.id())));
        button.setGraphic(tool.graphic());
        ofNullable(tool.descriptor().description()).filter(not(String::isBlank)).map(Tooltip::new)
                .ifPresent(button::setTooltip);
        return button;
    }

}
