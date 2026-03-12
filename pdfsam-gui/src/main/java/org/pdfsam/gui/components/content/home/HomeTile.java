/*
 * This file is part of the PDF Split And Merge source code
 * Created on 25 nov 2016
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.content.home;

import javafx.css.PseudoClass;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;

import static java.util.Objects.nonNull;

/**
 * Tile for the home panel
 *
 * @author Andrea Vacondio
 */
class HomeTile extends StackPane {
    private static final PseudoClass PSEUDO_CLASS_ARMED = PseudoClass.getPseudoClass("armed");
    private static final PseudoClass PSEUDO_CLASS_SELECTED = PseudoClass.getPseudoClass("selected");

    private final Button invisibleButton = new Button();

    public HomeTile(String title, String description, Node graphic, String categoryClass) {
        getStyleClass().addAll("home-tile");
        var top = new Label(title);
        top.getStyleClass().add("title");
        if (nonNull(graphic)) {
            top.setGraphic(graphic);
        }
        var bottom = new Label(description);
        bottom.setMinHeight(Region.USE_PREF_SIZE);
        bottom.getStyleClass().add("description");
        var vbox = new VBox(top, bottom);
        vbox.getStyleClass().add("right");

        invisibleButton.getStyleClass().add("home-tile-invisible-button");
        invisibleButton.setAccessibleText(title);
        invisibleButton.setAccessibleHelp(description);
        invisibleButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);

        var categoryMark = new Region();
        categoryMark.getStyleClass().addAll("category-mark", categoryClass);
        categoryMark.prefHeightProperty().bind(vbox.heightProperty());

        var hbox = new HBox(categoryMark, vbox);
        invisibleButton.prefHeightProperty().bind(hbox.heightProperty());
        invisibleButton.prefWidthProperty().bind(hbox.widthProperty());
        invisibleButton.setFocusTraversable(true);
        invisibleButton.focusedProperty().addListener((v, o, n) -> {
            pseudoClassStateChanged(PSEUDO_CLASS_SELECTED, n);
        });
        invisibleButton.armedProperty().addListener((v, o, n) -> {
            pseudoClassStateChanged(PSEUDO_CLASS_ARMED, n);
        });

        this.getChildren().addAll(hbox, invisibleButton);
    }

    public final boolean isArmed() {
        return invisibleButton.armedProperty().get();
    }

    public final void setOnAction(EventHandler<ActionEvent> eventHandler) {
        invisibleButton.setOnAction(eventHandler);
    }

    void addBottomPanel(Region pane) {
        StackPane.setAlignment(pane, Pos.BOTTOM_RIGHT);
        StackPane.setMargin(pane, new Insets(4));
        getChildren().add(pane);
    }
}
