/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/01/23
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

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.css.PseudoClass;
import javafx.scene.AccessibleRole;
import javafx.scene.control.Tooltip;
import org.kordamp.ikonli.javafx.FontIcon;
import org.pdfsam.model.ui.ContentItem;
import org.pdfsam.model.ui.SetActiveContentItemRequest;

import static java.util.Optional.ofNullable;
import static java.util.function.Predicate.not;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
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

    public String id() {
        return id;
    }

    /**
     * Property telling if the button is selected
     */
    private final BooleanProperty selected = new SimpleBooleanProperty(false) {
        @Override
        protected void invalidated() {
            pseudoClassStateChanged(SELECTED_PSEUDOCLASS_STATE, get());
            setDisable(get());
            if (get()) {
                setAccessibleRoleDescription(
                        SelectableSidebarButton.super.getText() + " (" + i18n().tr("selected") + ")");
            } else {
                setAccessibleRoleDescription(null);
            }
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
        if (item.graphic() instanceof FontIcon icon) {
            icon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        }
        button.setTooltip(new Tooltip(ofNullable(item.description()).filter(not(String::isBlank)).orElse(item.name())));
        return button;
    }

}
