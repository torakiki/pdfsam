/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 06/nov/2013
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
package org.pdfsam.gui.quickbar;

import java.io.IOException;

import javafx.beans.property.BooleanProperty;
import javafx.fxml.FXMLLoader;
import javafx.scene.Group;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.HBox;

import javax.annotation.PostConstruct;
import javax.inject.Named;

import org.pdfsam.ui.support.Style;
import org.springframework.core.io.ClassPathResource;

/**
 * @author Andrea Vacondio
 * 
 */
@Named
class ExpandButton extends HBox {
    private static final String LISTING_TOGGLE = "listing-toggle";
    private static final String LISTING_TOGGLE_SELECTED = "listing-toggle-selected";

    private ToggleButton toggle;

    ExpandButton() throws IOException {
        getStyleClass().addAll(Style.EXPAND_BOX.css());
        toggle = new ToggleButton();
        toggle.setGraphic((Group) FXMLLoader.load(new ClassPathResource("/fxml/Listing.fxml").getURL()));
        toggle.getStyleClass().add(LISTING_TOGGLE);
        toggle.selectedProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue) {
                toggle.getStyleClass().add(LISTING_TOGGLE_SELECTED);
            } else {
                toggle.getStyleClass().remove(LISTING_TOGGLE_SELECTED);
            }
        });
    }

    @PostConstruct
    private void init() {
        getChildren().add(toggle);
    }

    public final BooleanProperty selectedProperty() {
        return toggle.selectedProperty();
    }

}
