/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/lug/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
import org.sejda.model.pdf.PdfVersion;

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
        victim.addFilter(PdfVersion.VERSION_1_4);
        assertEquals(PdfVersion.VERSION_1_4, victim.requiredProperty().get());
    }

    @Test
    public void remove() {
        PdfVersionFilter victim = new PdfVersionFilter();
        victim.addFilter(PdfVersion.VERSION_1_4);
        assertEquals(PdfVersion.VERSION_1_4, victim.requiredProperty().get());
        victim.removeFilter(PdfVersion.VERSION_1_4);
        assertNotEquals(10, victim.requiredProperty().get());
    }

    @Test
    public void failingRemove() {
        PdfVersionFilter victim = new PdfVersionFilter();
        ChangeListener<? super PdfVersion> listener = mock(ChangeListener.class);
        victim.requiredProperty().addListener(listener);
        victim.removeFilter(PdfVersion.VERSION_1_4);
        verify(listener, never()).changed(any(ObservableValue.class), any(PdfVersion.class), any(PdfVersion.class));
    }

    @Test
    public void failingAdd() {
        PdfVersionFilter victim = new PdfVersionFilter();
        victim.addFilter(PdfVersion.VERSION_1_4);
        assertEquals(PdfVersion.VERSION_1_4, victim.requiredProperty().get());
        ChangeListener<? super PdfVersion> listener = mock(ChangeListener.class);
        victim.requiredProperty().addListener(listener);
        victim.addFilter(PdfVersion.VERSION_1_4);
        verify(listener, never()).changed(any(ObservableValue.class), any(PdfVersion.class), any(PdfVersion.class));
    }

    @Test
    public void keepMaxAsRequired() {
        PdfVersionFilter victim = new PdfVersionFilter();
        victim.addFilter(PdfVersion.VERSION_1_4);
        assertEquals(PdfVersion.VERSION_1_4, victim.requiredProperty().get());
        victim.addFilter(PdfVersion.VERSION_1_7);
        assertEquals(PdfVersion.VERSION_1_7, victim.requiredProperty().get());
        victim.addFilter(PdfVersion.VERSION_1_5);
        assertEquals(PdfVersion.VERSION_1_7, victim.requiredProperty().get());
        victim.removeFilter(PdfVersion.VERSION_1_7);
        assertEquals(PdfVersion.VERSION_1_5, victim.requiredProperty().get());
    }
}
