/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/01/23
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
import javafx.geometry.Pos;
import javafx.scene.AccessibleRole;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ContentDisplay;
import org.pdfsam.ui.components.support.Style;

/**
 * A button for the sidebar. It can show/hide the button label
 *
 * @author Andrea Vacondio
 */
class SidebarButton extends Button {

    public SidebarButton() {
        getStyleClass().addAll(Style.SIDEBAR_BUTTON.css());
        setMaxWidth(Double.MAX_VALUE);
        setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
        setAlignment(Pos.CENTER);
    }

    public SidebarButton(String text) {
        this();
        this.setText(text);
    }

    public SidebarButton(String text, Node graphic) {
        this(text);
        graphic.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        this.setGraphic(graphic);
    }

    private final BooleanProperty displayText = new SimpleBooleanProperty(false) {
        @Override
        protected void invalidated() {
            if (get()) {
                setContentDisplay(ContentDisplay.LEFT);
                setAlignment(Pos.CENTER_LEFT);
            } else {
                setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
                setAlignment(Pos.CENTER);
            }
        }
    };

    public final BooleanProperty displayTextProperty() {
        return displayText;
    }

    public final void setDisplayText(boolean value) {
        displayTextProperty().set(value);
    }

    public final boolean isDisplayText() {
        return displayText.get();
    }
}
