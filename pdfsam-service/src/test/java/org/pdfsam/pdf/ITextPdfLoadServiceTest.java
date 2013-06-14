/*
 * Created on 14/giu/2013
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.pdf;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Test;
import org.sejda.model.pdf.PdfMetadataKey;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

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
        List<PdfDocumentDescriptor> toLoad = new ArrayList<PdfDocumentDescriptor>();
        toLoad.add(descriptor);
        List<PdfDocumentDescriptor> result = victim.load(toLoad);
        assertEquals(1, result.size());
        PdfDocumentDescriptor item = result.get(0);
        assertNotNull(item);
        assertEquals(2, item.getPages());
        assertFalse(item.isEncrypted());
        assertEquals("Me", item.getMedatada(PdfMetadataKey.AUTHOR));
        assertEquals("test", item.getMedatada(PdfMetadataKey.KEYWORDS));
    }
}
