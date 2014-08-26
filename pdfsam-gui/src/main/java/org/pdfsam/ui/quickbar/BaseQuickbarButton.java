/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
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
package org.pdfsam.ui.quickbar;

import java.util.Objects;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.css.PseudoClass;
import javafx.scene.control.Button;
import javafx.scene.control.ContentDisplay;


/**
 * Base class for a quickbar button that can be selected and have its text displayed
 * 
 * @author Andrea Vacondio
 */
public class BaseQuickbarButton extends Button {
    private static final PseudoClass SELECTED_PSEUDOCLASS_STATE = PseudoClass.getPseudoClass("selected");

    public BaseQuickbarButton() {
        getStyleClass().addAll("quickbar-navigation-button");
        setMaxWidth(Double.MAX_VALUE);
        setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
        graphicProperty().addListener((o, oldVal, newVal) -> {
            if (Objects.nonNull(newVal)) {
                newVal.getStyleClass().add("quickbar-navigation-button-graphic");
            }
        });
    }

    private BooleanProperty displayText = new SimpleBooleanProperty(false) {
        @Override
        protected void invalidated() {
            if (get()) {
                setContentDisplay(ContentDisplay.LEFT);
            } else {
                setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
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
    private BooleanProperty selected = new SimpleBooleanProperty(false) {
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
