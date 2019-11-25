/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23/ott/2013
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import org.pdfsam.support.validation.Validator;
import org.pdfsam.ui.support.FXValidationSupport;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;

import javafx.animation.Animation.Status;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.geometry.Point2D;
import javafx.scene.Scene;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.stage.Window;
import javafx.util.Duration;

/**
 * {@link TextField} triggering validation when Enter key is pressed or when focus is lost. A {@link ValidationState} property is exposed to bind to the validation state. Default
 * implementation behaves as any value in the field is always valid, a different {@link Validator} can be set to achieve custom validation.
 * 
 * @author Andrea Vacondio
 * 
 */
public class ValidableTextField extends TextField {
    private static final KeyCombination ENTER_COMBO = new KeyCodeCombination(KeyCode.ENTER);

    private final FXValidationSupport<String> validationSupport = new FXValidationSupport<>();
    private ErrorTooltipManager errorTooltipManager;

    public ValidableTextField() {
        this("");
    }

    public ValidableTextField(String text) {
        super(text);
        this.getStyleClass().add("validable-field");
        focusedProperty().addListener((o, oldVal, newVal) -> {
            if (!newVal) {
                validate();
            }
        });

        textProperty().addListener((o, oldVal, newVal) -> validationSupport.makeNotValidated());
        validationSupport.validationStateProperty().addListener(
                o -> {
                    if (validationSupport.validationStateProperty().get() == ValidationState.INVALID
                            && errorTooltipManager != null) {
                        errorTooltipManager.showTooltip();
                    }
                });
    }

    public final ValidationState getValidationState() {
        return validationSupport.validationStateProperty().get();
    }

    public final ReadOnlyObjectProperty<ValidationState> validProperty() {
        return validationSupport.validationStateProperty();
    }

    /**
     * the field is marked with the class "invalid" when the validation status is invalid to give the user visual feedback.
     */
    public void setEnableInvalidStyle(final boolean active) {
        validationSupport.validationStateProperty().addListener(o -> {
            if (active) {
                if (validationSupport.validationStateProperty().get() == ValidationState.INVALID) {
                    getStyleClass().addAll(Style.INVALID.css());
                } else {
                    getStyleClass().removeAll(Style.INVALID.css());
                }
            }
        });
    }

    /**
     * Set the error message to display as a tooltip if the status is invalid
     * 
     * @param message
     */
    public void setErrorMessage(String message) {
        if (isNotBlank(message)) {
            this.errorTooltipManager = new ErrorTooltipManager(message);
        } else {
            this.errorTooltipManager = null;
        }
    }

    public void setOnEnterValidation(final boolean active) {
        setOnKeyReleased(t -> {
            if (active && ENTER_COMBO.match(t)) {
                validate();
            }
        });
    }

    /**
     * Sets the validator for this field. It doesn't trigger a validation.
     * 
     * @param validator
     * @see org.pdfsam.ui.support.FXValidationSupport#setValidator(org.pdfsam.support.validation.Validator)
     */
    public void setValidator(Validator<String> validator) {
        requireNotNullArg(validator, "Validator cannot be null for ValidableTextField");
        validationSupport.setValidator(validator);
    }

    /**
     * Triggers a validation programmatically
     */
    public void validate() {
        validationSupport.validate(getText());
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
            requireNotBlank(message, "Tooltip message cannot be blank");
            this.tooltip = new Tooltip(message);
            this.tooltip.getStyleClass().add(ERROR_TOOLTIP_CLASS);
            hideTimer.getKeyFrames().add(new KeyFrame(new Duration(5000)));
            hideTimer.setOnFinished(e -> {
                tooltip.hide();
                ErrorTooltipManager.this.active = false;
            });
            activationTimer.getKeyFrames().add(new KeyFrame(new Duration(250)));
            activationTimer.setOnFinished(e -> {
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
