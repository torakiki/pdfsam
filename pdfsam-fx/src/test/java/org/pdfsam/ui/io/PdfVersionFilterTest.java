/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/lug/2014
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
package org.pdfsam.ui.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.Test;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfVersionFilterTest {

    @Test
    public void add() {
        PdfVersionFilter victim = new PdfVersionFilter();
        victim.addFilter(Integer.valueOf(10));
        assertEquals(10, victim.requiredProperty().get());
    }

    @Test
    public void remove() {
        PdfVersionFilter victim = new PdfVersionFilter();
        victim.addFilter(10);
        assertEquals(10, victim.requiredProperty().get());
        victim.removeFilter(10);
        assertNotEquals(10, victim.requiredProperty().get());
    }

    @Test
    public void failingRemove() {
        PdfVersionFilter victim = new PdfVersionFilter();
        ChangeListener<? super Number> listener = mock(ChangeListener.class);
        victim.requiredProperty().addListener(listener);
        victim.removeFilter(10);
        verify(listener, never()).changed(any(ObservableValue.class), any(Number.class), any(Number.class));
    }

    @Test
    public void failingAdd() {
        PdfVersionFilter victim = new PdfVersionFilter();
        victim.addFilter(Integer.valueOf(10));
        assertEquals(10, victim.requiredProperty().get());
        ChangeListener<? super Number> listener = mock(ChangeListener.class);
        victim.requiredProperty().addListener(listener);
        victim.addFilter(Integer.valueOf(10));
        verify(listener, never()).changed(any(ObservableValue.class), any(Number.class), any(Number.class));
    }

    @Test
    public void keepMaxAsRequired() {
        PdfVersionFilter victim = new PdfVersionFilter();
        victim.addFilter(10);
        assertEquals(10, victim.requiredProperty().get());
        victim.addFilter(50);
        assertEquals(50, victim.requiredProperty().get());
        victim.addFilter(20);
        assertEquals(50, victim.requiredProperty().get());
        victim.removeFilter(50);
        assertEquals(20, victim.requiredProperty().get());
    }
}
