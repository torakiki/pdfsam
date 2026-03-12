/*
 * This file is part of the PDF Split And Merge source code
 * Created on 06/nov/2013
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
package org.pdfsam.gui.components.sidebar;

import javafx.beans.property.BooleanProperty;
import javafx.geometry.Insets;
import javafx.scene.AccessibleRole;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.HBox;
import javafx.scene.shape.SVGPath;
import org.pdfsam.core.context.BooleanPersistentProperty;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Toggle button to expand/collapse the sidebar
 *
 * @author Andrea Vacondio
 */
class ExpandButton extends HBox {
    private final ToggleButton toggle = new ToggleButton();
    private final SVGPath expand = new SVGPath();

    public ExpandButton() {
        getStyleClass().add("sidebar-expand-button");
        toggle.getStyleClass().addAll("sidebar-expand-toggle");
        expand.setContent("M8.59 16.58L13.17 12L8.59 7.41L10 6l6 6l-6 6l-1.41-1.42Z");
        expand.getStyleClass().add("sidebar-button-arrow");
        expand.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        toggle.setGraphic(expand);
        toggle.selectedProperty().addListener((observable, oldValue, newValue) -> {
            app().persistentSettings().set(BooleanPersistentProperty.SIDEBAR_EXPANDED_STATE, newValue);
            if (newValue) {
                toggle.setRotate(180);
                toggle.setAccessibleText(i18n().tr("Collapse the sidebar"));
            } else {
                toggle.setRotate(0);
                toggle.setAccessibleText(i18n().tr("Expand the sidebar"));
            }
        });
        toggle.setSelected(app().persistentSettings().get(BooleanPersistentProperty.SIDEBAR_EXPANDED_STATE));
        HBox.setMargin(toggle, new Insets(0, 7, 0, 7));
        getChildren().add(toggle);
    }

    public final BooleanProperty selectedProperty() {
        return toggle.selectedProperty();
    }
}
