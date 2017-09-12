/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/ago/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.ui.selection.multiple;

import static org.hamcrest.Matchers.isEmptyString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.conversion.exception.ConversionException;

/**
 * @author Andrea Vacondio
 *
 */
public class SelectionTableRowDataTest {

    @Rule
    public ClearEventStudioRule eventStudioRule = new ClearEventStudioRule();

    @Test
    public void empty() throws ConversionException {
        PdfDocumentDescriptor descriptor = mock(PdfDocumentDescriptor.class);
        SelectionTableRowData victim = new SelectionTableRowData(descriptor);
        assertTrue(victim.toPageRangeSet().isEmpty());
    }

    @Test
    public void invalidate() {
        PdfDocumentDescriptor descriptor = mock(PdfDocumentDescriptor.class);
        SelectionTableRowData victim = new SelectionTableRowData(descriptor);
        victim.invalidate();
        verify(descriptor).release();
    }

    @Test
    public void duplicate() {
        PdfDocumentDescriptor descriptor = mock(PdfDocumentDescriptor.class);
        SelectionTableRowData victim = new SelectionTableRowData(descriptor);
        assertFalse(victim.reverse.get());
        assertEquals("1", victim.pace.get());
        assertThat(victim.pageSelection.get(), isEmptyString());
        victim.reverse.set(true);
        victim.pace.set("3");
        victim.pageSelection.set("4");
        SelectionTableRowData dupe = victim.duplicate();
        verify(descriptor).retain();
        assertTrue(dupe.reverse.get());
        assertEquals("3", dupe.pace.get());
        assertEquals("4", dupe.pageSelection.get());

    }
}
