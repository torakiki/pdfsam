/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/ott/2013
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
package org.pdfsam.ui.io;

import java.io.File;

import javafx.css.PseudoClass;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;

/**
 * {@link ValidableTextField} with attached a browse button to let the user select a file
 * 
 * @author Andrea Vacondio
 * 
 */
abstract class BrowsableField extends HBox {
    private static final PseudoClass SELECTED_PSEUDOCLASS_STATE = PseudoClass.getPseudoClass("selected");

    private Button browseButton;
    private ValidableTextField textField = new ValidableTextField();
    private HBox validableContainer;
    private String browseWindowTitle = DefaultI18nContext.getInstance().i18n("Select");

    public BrowsableField() {
        HBox.setHgrow(textField, Priority.ALWAYS);
        setAlignment(Pos.CENTER_LEFT);
        validableContainer = new HBox(textField);
        validableContainer.getStyleClass().add("validable-container");
        textField.getStyleClass().add("validable-container-field");
        browseButton = new Button(DefaultI18nContext.getInstance().i18n("Browse"));
        browseButton.getStyleClass().addAll(Style.BROWSE_BUTTON.css());
        browseButton.prefHeightProperty().bind(validableContainer.heightProperty());
        browseButton.setMaxHeight(USE_PREF_SIZE);
        browseButton.setMinHeight(USE_PREF_SIZE);
        browseButton.setAlignment(Pos.CENTER);
        HBox.setHgrow(validableContainer, Priority.ALWAYS);
        textField.validProperty().addListener((o, oldValue, newValue) -> {
            if ((newValue == ValidationState.INVALID)) {
                validableContainer.getStyleClass().addAll(Style.INVALID.css());
            } else {
                validableContainer.getStyleClass().removeAll(Style.INVALID.css());
            }
        });
        textField.focusedProperty().addListener(
                (o, oldVal, newVal) -> validableContainer.pseudoClassStateChanged(SELECTED_PSEUDOCLASS_STATE, newVal));
        getChildren().addAll(validableContainer, browseButton);
    }

    /**
     * @return the internal {@link ValidableTextField}
     */
    public ValidableTextField getTextField() {
        return textField;
    }

    public final void setGraphic(Node value) {
        validableContainer.getChildren().clear();
        if (value != null) {
            validableContainer.getChildren().add(value);
        }
        validableContainer.getChildren().add(textField);
    }

    Button getBrowseButton() {
        return browseButton;
    }

    String getBrowseWindowTitle() {
        return browseWindowTitle;
    }

    public void setBrowseWindowTitle(String browseWindowTitle) {
        this.browseWindowTitle = browseWindowTitle;
    }

    /**
     * Set the text from the given input file
     * 
     * @param inputFile
     */
    abstract void setTextFromFile(File inputFile);
}
