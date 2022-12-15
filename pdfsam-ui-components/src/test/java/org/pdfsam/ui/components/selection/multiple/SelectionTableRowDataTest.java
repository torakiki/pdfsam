/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08/ago/2014
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
package org.pdfsam.ui.components.selection.multiple;

import javafx.beans.property.SimpleIntegerProperty;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.test.ClearEventStudioExtension;
import org.sejda.conversion.exception.ConversionException;

import java.io.File;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ClearEventStudioExtension.class)
public class SelectionTableRowDataTest {

    @Test
    public void empty() throws ConversionException {
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        SelectionTableRowData victim = new SelectionTableRowData(descriptor);
        assertTrue(victim.toPageRangeSet().isEmpty());
    }

    @Test
    public void invalidate() {
        var descriptor = mock(PdfDocumentDescriptor.class);
        when(descriptor.pages()).thenReturn(new SimpleIntegerProperty().asObject());
        SelectionTableRowData victim = new SelectionTableRowData(descriptor);
        victim.invalidate();
        verify(descriptor).release();
    }

    @Test
    public void duplicate() {
        var descriptor = mock(PdfDocumentDescriptor.class);
        when(descriptor.pages()).thenReturn(new SimpleIntegerProperty().asObject());
        var victim = new SelectionTableRowData(descriptor);
        assertFalse(victim.reverse.get());
        assertEquals("1", victim.pace.get());
        assertThat(victim.pageSelection.get()).isEmpty();
        victim.reverse.set(true);
        victim.pace.set("3");
        victim.pageSelection.set("4");
        victim.selectedPages.set(6);
        SelectionTableRowData dupe = victim.duplicate();
        verify(descriptor).retain();
        assertTrue(dupe.reverse.get());
        assertEquals("3", dupe.pace.get());
        assertEquals("4", dupe.pageSelection.get());
        assertEquals(6, dupe.selectedPages.get());
    }

    @Test
    public void selectedPagesNoRange() {
        var descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        var victim = new SelectionTableRowData(descriptor);
        assertEquals(0, victim.selectedPages.get());
        descriptor.pages(15);
        assertEquals(15, victim.selectedPages.get());
    }

    @Test
    public void selectedPagesZeroRange() {
        var descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        var victim = new SelectionTableRowData(descriptor);
        descriptor.pages(15);
        assertEquals(15, victim.selectedPages.get());
        victim.pageSelection.set("0");
        assertEquals(0, victim.selectedPages.get());
    }

    @Test
    public void selectedPagesRanges() {
        var descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        var victim = new SelectionTableRowData(descriptor);
        descriptor.pages(15);
        assertEquals(15, victim.selectedPages.get());
        victim.pageSelection.set("2-5,10,13-");
        assertEquals(8, victim.selectedPages.get());
    }

    @Test
    public void selectedInvalidPagesRanges() {
        var descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        var victim = new SelectionTableRowData(descriptor);
        descriptor.pages(15);
        assertEquals(15, victim.selectedPages.get());
        victim.pageSelection.set("Chuck Norris");
        assertEquals(0, victim.selectedPages.get());
    }
}
