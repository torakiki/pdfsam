/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/ago/2014
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
package org.pdfsam.ui.selection.multiple;

import static org.hamcrest.Matchers.any;
import static org.hamcrest.Matchers.isEmptyString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.Arrays;

import org.junit.Test;
import org.pdfsam.pdf.PdfDocumentDescriptor;

/**
 * @author Andrea Vacondio
 *
 */
public class LongColumnTest {

    @Test
    public void comparators() {
        Arrays.stream(LongColumn.values()).forEach(v -> assertEquals(-1, v.comparator().compare(1, 10)));
    }

    @Test
    public void getNullTextValue() {
        Arrays.stream(LongColumn.values()).forEach(v -> assertThat(v.getTextValue(null), isEmptyString()));
    }

    @Test
    public void size() {
        assertEquals("1 MB", LongColumn.SIZE.getTextValue(1024 * 1024));
    }

    @Test
    public void lastModified() {
        assertThat(LongColumn.LAST_MODIFIED.getTextValue(System.currentTimeMillis()), any(String.class));
    }

    @Test
    public void getSizeObservableValue() {
        File file = mock(File.class);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        SelectionTableRowData data = new SelectionTableRowData(descriptor);
        when(file.length()).thenReturn(Long.MAX_VALUE);
        assertEquals(Long.MAX_VALUE, LongColumn.SIZE.getObservableValue(data).getValue());
    }

    @Test
    public void getLastModifiedObservableValue() {
        File file = mock(File.class);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        SelectionTableRowData data = new SelectionTableRowData(descriptor);
        when(file.lastModified()).thenReturn(Long.MAX_VALUE);
        assertEquals(Long.MAX_VALUE, LongColumn.LAST_MODIFIED.getObservableValue(data).getValue());
    }
}
