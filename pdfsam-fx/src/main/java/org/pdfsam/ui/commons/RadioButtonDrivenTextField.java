/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/giu/2014
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
package org.pdfsam.ui.commons;

import javafx.geometry.Pos;
import javafx.scene.control.RadioButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;

import org.pdfsam.support.validation.Validator;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;

/**
 * Panel containing a {@link RadioButton} that, when selected, activates a text field.
 * 
 * @author Andrea Vacondio
 *
 */
public class RadioButtonDrivenTextField extends HBox {

    private RadioButton radio = new RadioButton();
    private ValidableTextField field = new ValidableTextField();

    public RadioButtonDrivenTextField(String radioText) {
        super(5);
        setAlignment(Pos.BOTTOM_LEFT);
        radio.setText(radioText);
        field.setPrefWidth(300);
        field.setDisable(true);
        field.setOnEnterValidation(true);
        field.setEnableInvalidStyle(true);
        radio.selectedProperty().addListener((o, oldVal, newVal) -> {
            field.setDisable(!newVal);
            if (newVal) {
                field.requestFocus();
            }
        });
        getChildren().addAll(radio, field);
    }

    public final void setToggleGroup(ToggleGroup value) {
        radio.setToggleGroup(value);
    }

    public final void setTooltip(Tooltip value) {
        radio.setTooltip(value);
    }

    public void setValidator(Validator<String> validator) {
        field.setValidator(validator);
    }

    public void validate() {
        field.validate();
    }

    public final ValidationState getValidationState() {
        return field.getValidationState();
    }

    public final void setPromptText(String value) {
        field.setPromptText(value);
    }

    public final String getText() {
        return field.getText();
    }

    public void setErrorMessage(String message) {
        field.setErrorMessage(message);
    }

    public final boolean isSelected() {
        return radio.isSelected();
    }

}
