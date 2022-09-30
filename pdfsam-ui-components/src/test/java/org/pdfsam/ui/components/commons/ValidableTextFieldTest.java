/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/lug/2014
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
package org.pdfsam.ui.components.commons;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.support.validation.Validators;
import org.pdfsam.ui.components.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.components.support.Style;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.testfx.api.FxAssert.verifyThat;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
public class ValidableTextFieldTest {
    private ValidableTextField victim;

    @Start
    public void start(Stage stage) {
        victim = new ValidableTextField();
        victim.setErrorMessage("Roundhouse kick!");
        Scene scene = new Scene(new HBox(victim, new Button("BUTTON")));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void nullValidator() {
        assertThrows(IllegalArgumentException.class, () -> victim.setValidator(null));
    }

    @Test
    public void validateOnFocusChange(FxRobot robot) {
        victim.setErrorMessage("");
        victim.setValidator(Validators.nonBlank());
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        robot.clickOn(victim);
        // focus lost
        robot.push(KeyCode.TAB);
        verifyThat(victim, (v) -> ValidationState.INVALID == v.getValidationState());
    }

    @Test
    public void invalidCssApplied(FxRobot robot) {
        victim.setValidator(Validators.nonBlank());
        victim.setEnableInvalidStyle(true);
        robot.clickOn(victim);
        // focus lost
        robot.push(KeyCode.TAB);
        verifyThat(victim, (v) -> v.getStyleClass().containsAll(Arrays.asList(Style.INVALID.css())));
    }

    @Test
    public void invalidCssNotApplied(FxRobot robot) {
        victim.setEnableInvalidStyle(false);
        robot.clickOn(victim);
        // focus lost
        robot.push(KeyCode.TAB);
        verifyThat(victim, (v) -> !v.getStyleClass().containsAll(Arrays.asList(Style.INVALID.css())));
    }

    @Test
    public void validateOnEnter(FxRobot robot) {
        victim.setValidator(Validators.nonBlank());
        victim.setOnEnterValidation(true);
        robot.clickOn(victim);
        // focus lost
        robot.push(KeyCode.ENTER);
        // focus lost
        verifyThat(victim, (v) -> ValidationState.INVALID == v.getValidationState());
    }

    @Test
    public void dontValidateOnEnter(FxRobot robot) {
        victim.setOnEnterValidation(false);
        robot.clickOn(victim);
        // focus lost
        robot.push(KeyCode.ENTER);
        verifyThat(victim, (v) -> ValidationState.NOT_VALIDATED == v.getValidationState());
    }

    @Test
    public void tooltip(FxRobot robot) {
        victim.setValidator(Validators.nonBlank());
        robot.clickOn(victim);
        // focus lost
        robot.push(KeyCode.TAB);
        robot.sleep(500);
        assertTrue(robot.listWindows().size() > 1);
    }
}
