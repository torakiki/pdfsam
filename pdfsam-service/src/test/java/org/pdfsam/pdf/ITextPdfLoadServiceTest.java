/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/giu/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Test;
import org.sejda.model.pdf.PdfMetadataKey;

/**
 * @author Andrea Vacondio
 * 
 */
public class ITextPdfLoadServiceTest {
    private ITextPdfLoadService victim = new ITextPdfLoadService();
    private File testFile;

    @Before
    public void setUp() throws IOException {
        testFile = File.createTempFile("PDFsamTest", ".pdf");
        testFile.deleteOnExit();
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/test_pdfsam.pdf"), testFile);
    }

    @Test
    public void load() {
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(testFile);
        List<PdfDocumentDescriptor> toLoad = new ArrayList<>();
        toLoad.add(descriptor);
        victim.load(toLoad);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        assertEquals(2, item.pagesPropery().get());
        assertEquals(EncryptionStatus.NOT_ENCRYPTED, item.encryptionStatusProperty().get());
        assertEquals("Me", item.getInformation(PdfMetadataKey.AUTHOR.getKey()));
        assertEquals("test", item.getInformation(PdfMetadataKey.KEYWORDS.getKey()));
    }
}
