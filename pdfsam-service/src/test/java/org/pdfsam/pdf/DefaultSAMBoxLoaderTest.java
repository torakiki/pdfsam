/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/feb/2015
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

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.GregorianCalendar;

import org.junit.Before;
import org.junit.Test;
import org.sejda.model.pdf.PdfMetadataKey;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.sambox.pdmodel.PDDocument;
import org.sejda.sambox.pdmodel.PDDocumentInformation;

/**
 * @author Andrea Vacondio
 *
 */
public class DefaultSAMBoxLoaderTest {
    private PDDocument document;
    private PDDocumentInformation info;
    private PdfDocumentDescriptor descriptor;

    @Before
    public void setUp() {
        document = mock(PDDocument.class);
        descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        info = mock(PDDocumentInformation.class);
        when(document.getDocumentInformation()).thenReturn(info);
        when(document.getVersion()).thenReturn("1.4");
        when(document.getNumberOfPages()).thenReturn(2);
        when(info.getAuthor()).thenReturn("Chuck Norris");
        when(info.getSubject()).thenReturn("Roundhause");
        when(info.getCreationDate()).thenReturn(new GregorianCalendar());
    }

    @Test
    public void accept() {
        new DefaultSAMBoxLoader().accept(document, descriptor);
        assertEquals(2, descriptor.pages().getValue().intValue());
        assertEquals(PdfVersion.VERSION_1_4, descriptor.getVersion());
        assertEquals("Chuck Norris", descriptor.getInformation(PdfMetadataKey.AUTHOR.getKey()));
        assertEquals("Roundhause", descriptor.getInformation(PdfMetadataKey.SUBJECT.getKey()));
        assertNotNull(descriptor.getInformation("FormattedCreationDate"));
        assertTrue(isEmpty(descriptor.getInformation(PdfMetadataKey.KEYWORDS.getKey())));
        assertTrue(isEmpty(descriptor.getInformation(PdfMetadataKey.CREATOR.getKey())));
        assertTrue(isEmpty(descriptor.getInformation(PdfMetadataKey.TITLE.getKey())));
    }

}
