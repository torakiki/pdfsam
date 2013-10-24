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
import javafx.animation.Animation.Status;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.NodeOrientation;
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

/**
 * {@link TextField} triggering validation when Enter key is pressed or when focus is lost. A boolean property is exposed to bind to the validation state.
 * 
 * @author Andrea Vacondio
 * 
 */
public class ValidableTextField extends TextField {

    private static final String ERROR_CLASS = "invalid";

    private FXValidationSupport<String> validationSupport;
    private ErrorTooltipManager errorTooltipManager;

    public ValidableTextField(Validator<String> validator) {
        this(validator, "");
    }

    public ValidableTextField(Validator<String> validator, String text) {
        super(text);
        requireNotNull(validator, "Validator cannot be null for ValidableTextField");
        this.validationSupport = new FXValidationSupport<>(validator);
        focusedProperty().addListener(new OnFocusLost());
        setOnKeyReleased(new OnEnterPressed());
        textProperty().addListener(new ResetStyleOnChangeText());
        validationSupport.validProperty().addListener(new StyleOnValidationStateChange());
        validate();
    }

    public final boolean isValid() {
        return validationSupport.validProperty().get();
    }

    public final ReadOnlyBooleanProperty validProperty() {
        return validationSupport.validProperty();
    }

    public void setErrorMessage(String message) {
        this.errorTooltipManager = new ErrorTooltipManager(message);

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
    private class StyleOnValidationStateChange implements ChangeListener<Boolean> {
        @Override
        public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
            if (!newValue && !getStyleClass().contains(ERROR_CLASS)) {
                getStyleClass().add(ERROR_CLASS);
                if (errorTooltipManager != null) {
                    errorTooltipManager.showTooltip();
                }
            }
            if (newValue && getStyleClass().contains(ERROR_CLASS)) {
                getStyleClass().remove(ERROR_CLASS);
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
            if (!isValid() && getStyleClass().contains(ERROR_CLASS)) {
                getStyleClass().remove(ERROR_CLASS);
            }
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

            if (ValidableTextField.this.getEffectiveNodeOrientation() == NodeOrientation.RIGHT_TO_LEFT) {
                anchorX = anchorX + ValidableTextField.this.getWidth() - tooltip.getWidth();
            }
            return new Point2D(anchorX, anchorY);
        }
    }
}
