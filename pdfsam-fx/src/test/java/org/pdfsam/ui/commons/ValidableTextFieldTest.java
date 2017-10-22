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
package org.pdfsam.ui.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.loadui.testfx.Assertions.verifyThat;

import java.util.Arrays;

import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;

import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class ValidableTextFieldTest extends GuiTest {
    private ValidableTextField victim;

    @Override
    protected Parent getRootNode() {
        victim = new ValidableTextField();
        victim.setErrorMessage("Roundhouse kick!");
        return new HBox(victim, new Button("BUTTON"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullValidator() {
        victim.setValidator(null);
    }

    @Test
    public void validateOnFocusChange() {
        victim.setErrorMessage("");
        victim.setValidator(Validators.nonBlank());
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click(victim);
        // focus lost
        push(KeyCode.TAB);
        verifyThat(victim, (v) -> ValidationState.INVALID == v.getValidationState());
    }

    @Test
    public void invalidCssApplied() {
        victim.setValidator(Validators.nonBlank());
        victim.setEnableInvalidStyle(true);
        click(victim);
        // focus lost
        push(KeyCode.TAB);
        verifyThat(victim, (v) -> v.getStyleClass().containsAll(Arrays.asList(Style.INVALID.css())));
    }

    @Test
    public void invalidCssNotApplied() {
        victim.setEnableInvalidStyle(false);
        click(victim);
        // focus lost
        push(KeyCode.TAB);
        verifyThat(victim, (v) -> !v.getStyleClass().containsAll(Arrays.asList(Style.INVALID.css())));
    }

    @Test
    public void validateOnEnter() {
        victim.setValidator(Validators.nonBlank());
        victim.setOnEnterValidation(true);
        click(victim);
        // focus lost
        push(KeyCode.ENTER);
        verifyThat(victim, (v) -> ValidationState.INVALID == v.getValidationState());
    }

    @Test
    public void dontValidateOnEnter() {
        victim.setOnEnterValidation(false);
        click(victim);
        // focus lost
        push(KeyCode.ENTER);
        verifyThat(victim, (v) -> ValidationState.NOT_VALIDATED == v.getValidationState());
    }

    @Test
    public void tooltip() {
        victim.setValidator(Validators.nonBlank());
        click(victim);
        // focus lost
        push(KeyCode.TAB);
        sleep(500);
        assertTrue(getWindows().size() > 1);
        // wait the tooltip to disappear
        sleep(4500);
    }
}
