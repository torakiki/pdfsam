/*
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
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
package org.pdfsam.gui.components.quickbar;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.css.PseudoClass;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.ContentDisplay;
import org.pdfsam.ui.components.support.Style;

/**
 * Base class for a quickbar button that can be selected and have its text displayed
 *
 * @author Andrea Vacondio
 */
public class BaseQuickbarButton extends Button {
    private static final PseudoClass SELECTED_PSEUDOCLASS_STATE = PseudoClass.getPseudoClass("selected");

    public BaseQuickbarButton() {
        getStyleClass().addAll(Style.QUICKBAR_BUTTON.css());
        setMaxWidth(Double.MAX_VALUE);
        setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
        setAlignment(Pos.CENTER);
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

    public final void setSelected(boolean value) {
        selectedProperty().set(value);
    }

    public final boolean isSelected() {
        return selected.get();
    }
}
