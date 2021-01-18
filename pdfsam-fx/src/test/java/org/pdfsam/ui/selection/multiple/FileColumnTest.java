/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 06/ago/2014
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
package org.pdfsam.ui.selection.multiple;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.isEmptyString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
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
        assertTrue(FileColumn.NAME.comparator().compare(new File("1192name.pdf"), new File("chuck.norris")) < 0);
        assertTrue(FileColumn.NAME.comparator().compare(new File("chuck.norris"), new File("1192name.pdf")) > 0);
        assertTrue(FileColumn.NAME.comparator().compare(new File("a.pdf"), new File("b.pdf")) < 0);
        assertTrue(FileColumn.NAME.comparator().compare(new File("b.pdf"), new File("a.pdf")) > 0);
        assertTrue(FileColumn.NAME.comparator().compare(new File("a.pdf"), new File("a.pdf")) == 0);
        assertEquals(-1, FileColumn.NAME.comparator().compare(new File("1_name"), new File("2_name")));
        assertEquals(-1, FileColumn.NAME.comparator().compare(new File("001_name"), new File("1_name")));
    }
}
