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
package org.pdfsam.ui.support;

import static org.hamcrest.core.IsEqual.equalTo;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.booleanThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

import org.junit.Test;
import org.pdfsam.support.validation.Validators;

/**
 * @author Andrea Vacondio
 * 
 */
@SuppressWarnings("unchecked")
public class FXValidationSupportTest {


    @Test
    public void startingValidState() {
        FXValidationSupport<String> victim = new FXValidationSupport<>(Validators.newNonBlankString());
        assertTrue(victim.validProperty().get());
        ChangeListener<Boolean> listener = mock(ChangeListener.class);
        victim.validProperty().addListener(listener);
        victim.validate("Chuck");
        verify(listener, never()).changed(any(ObservableValue.class), anyBoolean(), anyBoolean());
        assertTrue(victim.validProperty().get());
    }

    @Test
    public void bind() {
        FXValidationSupport<String> victim = new FXValidationSupport<>(Validators.newNonBlankString());
        ChangeListener<Boolean> listener = mock(ChangeListener.class);
        victim.validProperty().addListener(listener);
        victim.validate("Chuck");
        verify(listener, never()).changed(any(ObservableValue.class), anyBoolean(), anyBoolean());
        assertTrue(victim.validProperty().get());
        victim.validate("");
        verify(listener, times(1)).changed(any(ObservableValue.class), booleanThat(equalTo(Boolean.TRUE)),
                booleanThat(equalTo(Boolean.FALSE)));
        assertFalse(victim.validProperty().get());
    }

    @Test
    public void alwaysValid() {
        FXValidationSupport<String> victim = FXValidationSupport.alwaysValid();
        assertTrue(victim.validProperty().get());
        victim.validate("Chuck");
        assertTrue(victim.validProperty().get());
        victim.validate("");
        assertTrue(victim.validProperty().get());
    }

}
