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

import java.util.Collection;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.BooleanPropertyBase;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;

import org.pdfsam.module.Module;

/**
 * Panel containing quick access button to open modules
 * 
 * @author Andrea Vacondio
 *
 */
class ModulesPane extends VBox {
    private static final int MAX_MODULES = 4;
    private Label title;

    ModulesPane(String title) {
        this.getStyleClass().add("quickbar-modules");
        this.title = new Label(title);
        this.title.getStyleClass().add("quickbar-modules-title");
        getChildren().add(this.title);
        this.title.managedProperty().bind(displayText);
        this.title.visibleProperty().bind(displayText);

    }

    void initFor(Collection<Module> modules) {
        int modulesAdded = 0;
        for (Module current : modules) {
            ModuleButton currentButton = new ModuleButton(current);
            getChildren().add(currentButton);
            modulesAdded++;
            currentButton.displayTextProperty().bind(displayText);
            if (modulesAdded >= MAX_MODULES) {
                break;
            }
        }
    }

    /**
     * Property telling if the buttons labels should be visible
     */
    private BooleanProperty displayText = new BooleanPropertyBase(false) {

        public String getName() {
            return "displayText";
        }

        public Object getBean() {
            return ModulesPane.this;
        }
    };

    public final void setDisplayText(boolean value) {
        displayTextProperty().set(value);
    }

    public final boolean isDisplayText() {
        return displayText.get();
    }

    public final BooleanProperty displayTextProperty() {
        return displayText;
    }

}
