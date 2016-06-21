/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 06/ago/2014
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

import static org.hamcrest.Matchers.isEmptyString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

import java.io.File;

import org.junit.Test;
import org.pdfsam.pdf.PdfDocumentDescriptor;

/**
 * @author Andrea Vacondio
 *
 */
public class PageRangesColumnTest {

    @Test
    public void getObservableValue() {
        File file = mock(File.class);
        SelectionTableRowData data = new SelectionTableRowData(PdfDocumentDescriptor.newDescriptorNoPassword(file));
        data.setPageSelection("2");
        assertEquals("2", new PageRangesColumn().getObservableValue(data).getValue());
    }

    @Test
    public void getNullTextValue() {
        assertThat(new PageRangesColumn().getTextValue(null), isEmptyString());
    }

    @Test
    public void comparator() {
        assertEquals(-1, new PageRangesColumn().comparator().compare("1", "2"));
    }

}
