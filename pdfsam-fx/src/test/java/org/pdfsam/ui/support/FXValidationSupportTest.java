/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23/ott/2013
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
package org.pdfsam.ui.support;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

/**
 * @author Andrea Vacondio
 * 
 */
@SuppressWarnings("unchecked")
public class FXValidationSupportTest {

    @Test
    public void startingState() {
        FXValidationSupport<String> victim = new FXValidationSupport<>();
        victim.setValidator(Validators.nonBlank());
        assertEquals(ValidationState.NOT_VALIDATED, victim.validationStateProperty().get());
        ChangeListener<ValidationState> listener = mock(ChangeListener.class);
        victim.validationStateProperty().addListener(listener);
        victim.validate("Chuck");
        verify(listener, times(1)).changed(any(ObservableValue.class), eq(ValidationState.NOT_VALIDATED),
                eq(ValidationState.VALID));
        assertEquals(ValidationState.VALID, victim.validationStateProperty().get());
    }

    @Test
    public void behaviour() {
        FXValidationSupport<String> victim = new FXValidationSupport<>();
        victim.setValidator(Validators.nonBlank());
        ChangeListener<ValidationState> listener = mock(ChangeListener.class);
        victim.validationStateProperty().addListener(listener);
        victim.validate("Chuck");
        verify(listener, times(1)).changed(any(ObservableValue.class), eq(ValidationState.NOT_VALIDATED),
                eq(ValidationState.VALID));
        assertEquals(ValidationState.VALID, victim.validationStateProperty().get());
        victim.validate(" ");
        verify(listener, times(1)).changed(any(ObservableValue.class), eq(ValidationState.VALID),
                eq(ValidationState.INVALID));
        assertEquals(ValidationState.INVALID, victim.validationStateProperty().get());
    }

    @Test
    public void alwaysValid() {
        FXValidationSupport<String> victim = new FXValidationSupport<>();
        victim.validate("Chuck");
        assertEquals(ValidationState.VALID, victim.validationStateProperty().get());
        victim.validate("");
        assertEquals(ValidationState.VALID, victim.validationStateProperty().get());
    }

}
