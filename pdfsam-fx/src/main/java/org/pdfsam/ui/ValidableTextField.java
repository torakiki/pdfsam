/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23/ott/2013
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
package org.pdfsam.ui;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.support.RequireUtils.require;
import static org.pdfsam.support.RequireUtils.requireNotNull;

import java.util.Arrays;

import javafx.animation.Animation.Status;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Point2D;
import javafx.scene.Scene;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.KeyEvent;
import javafx.stage.Window;
import javafx.util.Duration;

import org.pdfsam.support.validation.Validator;
import org.pdfsam.ui.support.FXValidationSupport;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;

/**
 * {@link TextField} triggering validation when Enter key is pressed or when focus is lost. A {@link ValidationState} property is exposed to bind to the validation state. Default
 * implementation behaves as any value in the field is always valid, a different {@link Validator} can be set to achieve custom validation.
 * 
 * @author Andrea Vacondio
 * 
 */
public class ValidableTextField extends TextField {

    private final FXValidationSupport<String> validationSupport = new FXValidationSupport<>();
    private ErrorTooltipManager errorTooltipManager;

    public ValidableTextField() {
        this("");
    }

    public ValidableTextField(String text) {
        super(text);
        focusedProperty().addListener(new OnFocusLost());
        setOnKeyReleased(new OnEnterPressed());
        textProperty().addListener(new ResetStyleOnChangeText());
        validationSupport.validationStateProperty().addListener(new StyleOnValidationStateChange());
    }

    public final ValidationState getValidationState() {
        return validationSupport.validationStateProperty().get();
    }

    public final ReadOnlyObjectProperty<ValidationState> validProperty() {
        return validationSupport.validationStateProperty();
    }

    public void setErrorMessage(String message) {
        this.errorTooltipManager = new ErrorTooltipManager(message);
    }

    /**
     * Sets the validator for this field and updates the validation state
     * 
     * @param validator
     * @see org.pdfsam.ui.support.FXValidationSupport#setValidator(org.pdfsam.support.validation.Validator)
     */
    public void setValidator(Validator<String> validator) {
        requireNotNull(validator, "Validator cannot be null for ValidableTextField");
        validationSupport.setValidator(validator);
        validate();
    }

    /**
     * Triggers a validation programmatically
     */
    public void validate() {
        validationSupport.validate(getText());
    }

    /**
     * Style the field based on its validation state
     * 
     * @author Andrea Vacondio
     * 
     */
    private class StyleOnValidationStateChange implements ChangeListener<ValidationState> {
        @Override
        public void changed(ObservableValue<? extends ValidationState> observable, ValidationState oldValue,
                ValidationState newValue) {
            if ((newValue == ValidationState.INVALID)
                    && !getStyleClass().containsAll(Arrays.asList(Style.INVALID.css()))) {
                getStyleClass().addAll(Style.INVALID.css());
                if (errorTooltipManager != null) {
                    errorTooltipManager.showTooltip();
                }
            }
            if ((newValue != ValidationState.INVALID)
                    && getStyleClass().containsAll(Arrays.asList(Style.INVALID.css()))) {
                getStyleClass().removeAll(Style.INVALID.css());
            }
        }
    }

    /**
     * Remove the Error style when the user starts typing again
     * 
     * @author Andrea Vacondio
     * 
     */
    private class ResetStyleOnChangeText implements ChangeListener<String> {

        public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
            validationSupport.makeNotValidated();
        }
    }

    /**
     * Trigger validation on focus lost
     * 
     * @author Andrea Vacondio
     * 
     */
    private class OnFocusLost implements ChangeListener<Boolean> {
        @Override
        public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
            if (!newValue) {
                validate();
            }
        }
    }

    /**
     * Trigger validation on EnterKey pressed
     * 
     * @author Andrea Vacondio
     * 
     */
    private class OnEnterPressed implements EventHandler<KeyEvent> {
        private final KeyCombination combo = new KeyCodeCombination(KeyCode.ENTER);

        public void handle(KeyEvent t) {
            if (combo.match(t)) {
                validate();
            }
        }
    }

    /**
     * Manages the show/hide of the error tooltip
     * 
     * @author Andrea Vacondio
     * 
     */
    private final class ErrorTooltipManager {

        private static final String ERROR_TOOLTIP_CLASS = "error-tooltip";

        private Tooltip tooltip;
        private boolean active = false;

        private Timeline activationTimer = new Timeline();
        private Timeline hideTimer = new Timeline();

        private ErrorTooltipManager(String message) {
            require(isNotBlank(message), "Tooltip message cannot be blank");
            this.tooltip = new Tooltip(message);
            this.tooltip.getStyleClass().add(ERROR_TOOLTIP_CLASS);
            hideTimer.getKeyFrames().add(new KeyFrame(new Duration(5000)));
            hideTimer.setOnFinished(new EventHandler<ActionEvent>() {
                @Override
                public void handle(ActionEvent event) {
                    tooltip.hide();
                    ErrorTooltipManager.this.active = false;
                }
            });
            activationTimer.getKeyFrames().add(new KeyFrame(new Duration(250)));
            activationTimer.setOnFinished(new EventHandler<ActionEvent>() {
                @Override
                public void handle(ActionEvent event) {
                    if (!ErrorTooltipManager.this.active) {
                        Scene scene = getScene();
                        if (scene != null) {
                            Window owner = scene.getWindow();
                            if (owner != null && owner.isShowing() && ValidableTextField.this.isVisible()) {

                                Point2D where = getDisplayCoordiantes(owner, scene);

                                tooltip.show(ValidableTextField.this, where.getX(), where.getY());
                                ErrorTooltipManager.this.active = true;
                                hideTimer.playFromStart();
                            }
                        }
                    }
                }
            });
        }

        private void showTooltip() {
            if (activationTimer.getStatus() != Status.RUNNING) {
                activationTimer.stop();
                activationTimer.playFromStart();
            }
        }

        private Point2D getDisplayCoordiantes(Window owner, Scene scene) {
            Point2D nodeCoord = ValidableTextField.this.localToScene(0.0, ValidableTextField.this.getHeight());
            double anchorX = Math.round(owner.getX() + scene.getX() + nodeCoord.getX() + 2);
            double anchorY = Math.round(owner.getY() + scene.getY() + nodeCoord.getY() - 2);
            return new Point2D(anchorX, anchorY);
        }
    }
}
