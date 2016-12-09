/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25 nov 2016
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

import static java.util.Objects.nonNull;

import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.ReadOnlyBooleanWrapper;
import javafx.css.PseudoClass;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;

/**
 * Base class for a dashboard tile
 * 
 * @author Andrea Vacondio
 *
 */
class DashboardTile extends Region {
    private static final PseudoClass ARMED_PSEUDOCLASS_STATE = PseudoClass.getPseudoClass("armed");

    VBox bottom = new VBox();
    private Button button = new Button();

    public DashboardTile(String title, String description, Node graphic) {
        getStyleClass().addAll("dashboard-modules-tile");
        Label titleLabel = new Label(title);
        titleLabel.getStyleClass().add("dashboard-modules-tile-title");
        if (nonNull(graphic)) {
            titleLabel.setGraphic(graphic);
        }
        Label textLabel = new Label(description);
        textLabel.getStyleClass().add("dashboard-modules-tile-text");
        textLabel.setMinHeight(USE_PREF_SIZE);
        VBox topTile = new VBox(5);
        topTile.getChildren().addAll(titleLabel, textLabel);

        button.getStyleClass().add("dashboard-modules-invisible-button");
        button.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);

        armed.bind(button.armedProperty());
        bottom.getChildren().addAll(new StackPane(topTile, button));
        bottom.getStyleClass().add("dashboard-modules-tile-inner");
        prefHeightProperty().bind(bottom.heightProperty());
        setMaxHeight(USE_PREF_SIZE);
        setMinHeight(USE_PREF_SIZE);
        getChildren().add(bottom);
    }

    /**
     * Property telling if the region (acting as a button) is armed
     */
    ReadOnlyBooleanWrapper armed = new ReadOnlyBooleanWrapper(false) {
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

    public final void setOnAction(EventHandler<ActionEvent> eventHandler) {
        button.setOnAction(eventHandler);
    }

    void addBottomPanel(Region pane) {
        bottom.getChildren().add(pane);
    }
}
