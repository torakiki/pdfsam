/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 24/mar/2014
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
package org.pdfsam.ui.workarea;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.HashSet;
import java.util.Set;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.layout.VBox;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.ui.event.SetActiveModuleRequest;
import org.sejda.eventstudio.annotation.EventListener;

/**
 * Panel showing buttons to access the most used and most recently used modules
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class QuickbarModuleButtons extends VBox {

    private Set<ModuleButton> buttons = new HashSet<>();

    @Inject
    QuickbarModuleButtons(QuickbarModuleButtonsProvider provider) {
        this.getStyleClass().add("quickbar-items");
        provider.buttons().forEach(b -> {
            b.displayTextProperty().bind(displayText);
            getChildren().add(b);
            this.buttons.add(b);
        });
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onSetCurrentModuleRequest(SetActiveModuleRequest r) {
        r.getActiveModuleId().ifPresent(id -> buttons.forEach((b) -> b.setSelected(b.moduleId().equals(id))));
    }

    /**
     * Property telling if the buttons labels should be visible
     */
    private BooleanProperty displayText = new SimpleBooleanProperty(false);

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
