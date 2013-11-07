/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.navigation;

import static org.pdfsam.support.RequireUtils.require;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.BooleanPropertyBase;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.bushe.swing.event.EventBus;
import org.pdfsam.gui.SetCurrentModuleRequest;
import org.pdfsam.module.Module;
import org.pdfsam.module.UsageService;
import org.pdfsam.ui.support.Style;

/**
 * Pane holding the most used modules quick access buttons
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class MostUsedModulesPane extends VBox {

    private static final int MAX_MODULES = 4;
    @Inject
    private UsageService usage;
    private List<ModuleButton> buttons = new ArrayList<>();

    @PostConstruct
    private void init() {
        for (Module current : usage.getMostUsed()) {
            ModuleButton currentButton = new ModuleButton(current);
            getChildren().add(currentButton);
            buttons.add(currentButton);
            if (buttons.size() >= MAX_MODULES) {
                break;
            }
        }
    }

    private BooleanProperty displayText = new BooleanPropertyBase(false) {
        {
            this.addListener(new ChangeListener<Boolean>() {
                public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
                    // TODO lambda
                    for (ModuleButton current : MostUsedModulesPane.this.buttons) {
                        current.displayText(newValue);
                    }
                }
            });
        }

        public String getName() {
            return "displayText";
        }

        public Object getBean() {
            return MostUsedModulesPane.this;
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

    /**
     * Button binded to a {@link Module}
     * 
     * @author Andrea Vacondio
     * 
     */
    private static final class ModuleButton extends Button {

        private Module module;

        private ModuleButton(Module module) {
            require(module != null, "Module cannot be null");
            this.module = module;
            setGraphic(this.module.graphic());
            getStyleClass().addAll(Style.TOOLBAR_NAVIGATION_BUTTON.css());
            setMaxWidth(Double.MAX_VALUE);
            // TODO lambda
            setOnAction(new EventHandler<ActionEvent>() {
                public void handle(ActionEvent event) {
                    EventBus.publish(new SetCurrentModuleRequest(ModuleButton.this.module.id()));
                }
            });
            setTooltip(new Tooltip(this.module.descriptor().getName()));
        }

        private void displayText(boolean display) {
            if (display) {
                setText(module.descriptor().getName());
            } else {
                setText("");
            }
        }

    }
}
