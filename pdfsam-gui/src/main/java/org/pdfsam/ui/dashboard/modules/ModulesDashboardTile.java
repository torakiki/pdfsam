/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/mag/2014
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
package org.pdfsam.ui.dashboard.modules;

import static org.pdfsam.ui.event.SetActiveModuleRequest.activeteModule;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.module.Module;
import org.pdfsam.ui.commons.OpenUrlRequest;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.ReadOnlyBooleanWrapper;
import javafx.css.PseudoClass;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;

/**
 * Tile for the dashboard modules pane. It displays details about the {@link Module} and by clicking it the module will be opened in the Workarea.
 * 
 * @author Andrea Vacondio
 *
 */
class ModulesDashboardTile extends Region {

    private static final PseudoClass ARMED_PSEUDOCLASS_STATE = PseudoClass.getPseudoClass("armed");

    private VBox topTile = new VBox(5);
    private Button button = new Button();
    private VBox toolButtons = new VBox(5);

    ModulesDashboardTile(Module module) {
        getStyleClass().addAll("dashboard-modules-tile");
        Label titleLabel = new Label(module.descriptor().getName());
        titleLabel.getStyleClass().add("dashboard-modules-tile-title");
        titleLabel.setGraphic(module.graphic());
        Label textLabel = new Label(module.descriptor().getDescription());
        textLabel.getStyleClass().add("dashboard-modules-tile-text");
        textLabel.setMinHeight(USE_PREF_SIZE);
        topTile.getChildren().addAll(titleLabel, textLabel);
        button.getStyleClass().add("dashboard-modules-invisible-button");
        button.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        button.setOnAction(e -> eventStudio().broadcast(activeteModule(module.id())));
        armed.bind(button.armedProperty());
        VBox inner = new VBox(new StackPane(topTile, button));
        prefHeightProperty().bind(inner.heightProperty());
        setMaxHeight(USE_PREF_SIZE);
        setMinHeight(USE_PREF_SIZE);
        inner.getStyleClass().add("dashboard-modules-tile-inner");
        getChildren().add(inner);
        module.descriptor().getSupportURL().ifPresent(url -> {
            Button playButton = GlyphsDude.createIconButton(MaterialDesignIcon.HELP_CIRCLE, "");
            playButton.getStyleClass().add("pdfsam-toolbar-button");
            playButton.setOnAction(e -> eventStudio().broadcast(new OpenUrlRequest(url)));
            toolButtons.getChildren().add(playButton);
            toolButtons.getStyleClass().add("dashboard-modules-toolbar");
            inner.getChildren().add(toolButtons);
        });
    }

    /**
     * Property telling if the region (acting as a button) is armed
     */
    private ReadOnlyBooleanWrapper armed = new ReadOnlyBooleanWrapper(false) {
        @Override
        protected void invalidated() {
            pseudoClassStateChanged(ARMED_PSEUDOCLASS_STATE, get());
        }
    };

    public final ReadOnlyBooleanProperty armedProperty() {
        return armed.getReadOnlyProperty();
    }

    public final boolean isArmed() {
        return armed.get();
    }
}
