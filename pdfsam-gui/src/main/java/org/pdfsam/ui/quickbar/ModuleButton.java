/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/feb/2014
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.pdfsam.support.RequireUtils.require;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.css.PseudoClass;
import javafx.scene.control.Button;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.Tooltip;

import org.pdfsam.module.Module;
import org.pdfsam.ui.SetCurrentModuleRequest;

/**
 * Button to open a module
 * 
 * @author Andrea Vacondio
 *
 */
class ModuleButton extends Button {
    private static final PseudoClass SELECTED_PSEUDOCLASS_STATE = PseudoClass.getPseudoClass("selected");

    private Module module;

    ModuleButton(Module module) {
        require(module != null, "Module cannot be null");
        this.module = module;
        setGraphic(this.module.graphic());
        setText(this.module.descriptor().getName());
        getStyleClass().addAll("quickbar-navigation-button");
        setMaxWidth(Double.MAX_VALUE);
        setOnAction(e -> eventStudio().broadcast(new SetCurrentModuleRequest(ModuleButton.this.module.id())));
        setTooltip(new Tooltip(this.module.descriptor().getDescription()));
    }

    String moduleId() {
        return module.id();
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
