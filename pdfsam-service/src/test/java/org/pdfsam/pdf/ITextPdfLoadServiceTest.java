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
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.AfterClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.module.RequiredPdfData;
import org.sejda.model.pdf.PdfMetadataKey;

/**
 * @author Andrea Vacondio
 * 
 */
public class ITextPdfLoadServiceTest {
    private ITextPdfLoadService victim = new ITextPdfLoadService(Arrays.asList(new PdfLoader[] {
            new DefaultPdfLoader(), new BookmarksLevelLoader() }));
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @AfterClass
    public static void afterClass() {
        eventStudio().clear();
    }

    @Test
    public void load() throws IOException {
        File testFile = folder.newFile("PDFsamTest.pdf");
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/test_pdfsam.pdf"), testFile);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(testFile);
        List<PdfDocumentDescriptor> toLoad = Arrays.asList(new PdfDocumentDescriptor[] { descriptor });
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        assertEquals(2, item.pages().getValue().intValue());
        assertEquals(PdfDescriptorLoadingStatus.LOADED, descriptor.loadingStatus().getValue());
        assertEquals("Me", item.getInformation(PdfMetadataKey.AUTHOR.getKey()));
        assertEquals("test", item.getInformation(PdfMetadataKey.KEYWORDS.getKey()));
    }

    @Test
    public void invalidPdf() throws IOException {
        File testFile = folder.newFile("PDFsamTest.pdf");
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/im_empty.pdf"), testFile);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(testFile);
        List<PdfDocumentDescriptor> toLoad = Arrays.asList(new PdfDocumentDescriptor[] { descriptor });
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        assertEquals(PdfDescriptorLoadingStatus.WITH_ERRORS, descriptor.loadingStatus().getValue());
    }

    @Test
    public void encNoPwd() throws IOException {
        File testFile = folder.newFile("PDFsamTest.pdf");
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/enc_test_pdfsam.pdf"), testFile);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(testFile);
        List<PdfDocumentDescriptor> toLoad = Arrays.asList(new PdfDocumentDescriptor[] { descriptor });
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        assertEquals(PdfDescriptorLoadingStatus.ENCRYPTED, descriptor.loadingStatus().getValue());
    }

    @Test
    public void encWithPwd() throws IOException {
        File testFile = folder.newFile("PDFsamTest.pdf");
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/enc_test_pdfsam.pdf"), testFile);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptor(testFile, "test");
        List<PdfDocumentDescriptor> toLoad = Arrays.asList(new PdfDocumentDescriptor[] { descriptor });
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        assertEquals(PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION, descriptor.loadingStatus().getValue());
    }

    @Test
    public void invalidate() throws IOException {
        File testFile = folder.newFile("PDFsamTest.pdf");
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/test_pdfsam.pdf"), testFile);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(testFile);
        List<PdfDocumentDescriptor> toLoad = Arrays.asList(new PdfDocumentDescriptor[] { descriptor });
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        descriptor.invalidate();
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        assertEquals(PdfDescriptorLoadingStatus.REQUESTED, descriptor.loadingStatus().getValue());
    }
}
