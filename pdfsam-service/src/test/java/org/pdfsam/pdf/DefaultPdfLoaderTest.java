/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/ago/2014
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
package org.pdfsam.pdf;

import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;

import org.junit.Before;
import org.junit.Test;
import org.sejda.model.pdf.PdfVersion;

import com.itextpdf.text.pdf.PdfReader;

/**
 * @author Andrea Vacondio
 *
 */
public class DefaultPdfLoaderTest {

    private PdfReader reader;
    private PdfDocumentDescriptor descriptor;
    private HashMap<String, String> info = new HashMap<>();

    @Before
    public void setUp() {
        reader = mock(PdfReader.class);
        descriptor = mock(PdfDocumentDescriptor.class);
        when(reader.getNumberOfPages()).thenReturn(2);
        when(reader.getPdfVersion()).thenReturn('4');
        info.clear();
        when(reader.getInfo()).thenReturn(info);
    }

    @Test
    public void accept() {
        new DefaultPdfLoader().accept(reader, descriptor);
        verify(descriptor).setPages(2);
        verify(descriptor).setVersion(PdfVersion.VERSION_1_4);
        verify(descriptor).setInformationDictionary(info);
        verify(descriptor, never()).putInformation(eq("FormattedCreationDate"), anyString());
    }

    @Test
    public void acceptWithCreationDate() {
        info.put("CreationDate", "D:20140120102520+01'00'");
        new DefaultPdfLoader().accept(reader, descriptor);
        verify(descriptor).setPages(2);
        verify(descriptor).setVersion(PdfVersion.VERSION_1_4);
        verify(descriptor).setInformationDictionary(info);
        verify(descriptor).putInformation(eq("FormattedCreationDate"), anyString());
    }

}
