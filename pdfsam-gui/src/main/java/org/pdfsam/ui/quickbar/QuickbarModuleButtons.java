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
package org.pdfsam.ui.quickbar;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.module.Module;
import org.pdfsam.module.UsageService;
import org.pdfsam.ui.SetCurrentModuleRequest;
import org.sejda.eventstudio.annotation.EventListener;

/**
 * Panel showing buttons to access the most used and most recently used modules
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class QuickbarModuleButtons extends VBox {
    private static final int RECENT_MODULES = 3;
    private static final int MAX_MODULES = 8;

    @Inject
    private UsageService usage;
    private List<ModuleButton> buttons = new ArrayList<>();

    QuickbarModuleButtons() {
        this.getStyleClass().add("quickbar-modules");
    }

    @PostConstruct
    void init() {
        LinkedHashSet<Module> collected = new LinkedHashSet<>();
        fillWithMostRecentlyUsed(collected);
        fillWithMostUsed(collected);
        for (Module current : collected) {
            ModuleButton currentButton = new ModuleButton(current);
            currentButton.displayTextProperty().bind(displayText);
            getChildren().add(currentButton);
            buttons.add(currentButton);
        }
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void on(SetCurrentModuleRequest r) {
        buttons.forEach((b) -> b.setSelected(b.moduleId().equals(r.getModuleId())));
    }

    private void fillWithMostUsed(LinkedHashSet<Module> collected) {
        for (Module current : usage.getMostUsed()) {
            collected.add(current);
            if (collected.size() >= MAX_MODULES) {
                break;
            }
        }
    }

    private void fillWithMostRecentlyUsed(LinkedHashSet<Module> collected) {
        for (Module current : usage.getMostRecentlyUsed()) {
            collected.add(current);
            if (collected.size() >= RECENT_MODULES) {
                break;
            }
        }
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
