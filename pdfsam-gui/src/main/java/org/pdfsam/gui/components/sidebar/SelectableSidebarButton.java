package org.pdfsam.gui.components.sidebar;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/01/23
 * Copyright 2023 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.css.PseudoClass;
import javafx.scene.control.Tooltip;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.ContentItem;
import org.pdfsam.model.ui.SetActiveContentItemRequest;

import static java.util.Optional.ofNullable;
import static java.util.function.Predicate.not;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * A sidebar button that can be selected.
 *
 * @author Andrea Vacondio
 */
public class SelectableSidebarButton extends SidebarButton implements Selectable {

    private static final PseudoClass SELECTED_PSEUDOCLASS_STATE = PseudoClass.getPseudoClass("selected");
    private final String id;

    SelectableSidebarButton(String id, String text) {
        super(text);
        requireNotBlank(id, "ID cannot be blank");
        this.id = id;
    }

    /**
     * Property telling if the button is selected
     */
    private final BooleanProperty selected = new SimpleBooleanProperty(false) {
        @Override
        protected void invalidated() {
            pseudoClassStateChanged(SELECTED_PSEUDOCLASS_STATE, get());
            setDisable(get());
        }
    };

    public final BooleanProperty selectedProperty() {
        return selected;
    }

    @Override
    public final void setSelected(boolean value) {
        selectedProperty().set(value);
    }

    @Override
    public final boolean isSelected() {
        return selected.get();
    }

    /**
     * Selects this button if its id is equals to value
     *
     * @param id
     */
    @Override
    public final void selectIf(String id) {
        selectedProperty().set(this.id.equals(id));
    }

    /**
     * Factory method for a {@link SelectableSidebarButton} created from a {@link ContentItem}
     */
    public static SelectableSidebarButton of(ContentItem item) {
        requireNotNullArg(item, "ContentItem cannot be null");
        var button = new SelectableSidebarButton(item.id(), item.name());
        button.setOnAction(e -> eventStudio().broadcast(new SetActiveContentItemRequest(item.id())));
        button.setGraphic(item.graphic());
        ofNullable(item.description()).filter(not(String::isBlank)).map(Tooltip::new).ifPresent(button::setTooltip);
        return button;
    }

    /**
     * Factory method for a {@link SelectableSidebarButton} created from a {@link "Tool cannot be null"}
     */
    public static SelectableSidebarButton of(Tool tool) {
        requireNotNullArg(tool, "Tool cannot be null");
        var button = new SelectableSidebarButton(tool.id(), tool.descriptor().name());
        button.setOnAction(e -> eventStudio().broadcast(new SetActiveContentItemRequest(tool.id())));
        button.setGraphic(tool.graphic());
        ofNullable(tool.descriptor().description()).filter(not(String::isBlank)).map(Tooltip::new)
                .ifPresent(button::setTooltip);
        return button;
    }
}
