/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 06/ago/2014
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
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;

import org.junit.Test;
import org.pdfsam.pdf.PdfDocumentDescriptor;

/**
 * @author Andrea Vacondio
 *
 */
public class FileColumnTest {
    @Test
    public void getObservableValue() {
        File file = mock(File.class);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        SelectionTableRowData data = new SelectionTableRowData(descriptor);
        assertEquals(file, FileColumn.NAME.getObservableValue(data).getValue());
    }

    @Test
    public void getNullTextValue() {
        assertThat(FileColumn.NAME.getTextValue(null), isEmptyString());
    }

    @Test
    public void getTextValue() {
        File file = mock(File.class);
        when(file.getName()).thenReturn("name");
        assertEquals("name", FileColumn.NAME.getTextValue(file));
    }

    @Test
    public void comparator() {
        File file1 = mock(File.class);
        when(file1.getName()).thenReturn("1_name");
        File file2 = mock(File.class);
        when(file2.getName()).thenReturn("2_name");
        assertEquals(-1, FileColumn.NAME.comparator().compare(file1, file2));
    }
}
