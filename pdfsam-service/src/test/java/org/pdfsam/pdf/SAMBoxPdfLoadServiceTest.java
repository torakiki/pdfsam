/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mar/2015
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
package org.pdfsam.pdf;

import static com.google.code.tempusfugit.temporal.Duration.seconds;
import static com.google.code.tempusfugit.temporal.Timeout.timeout;
import static com.google.code.tempusfugit.temporal.WaitFor.waitOrTimeout;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeoutException;

import org.apache.commons.io.FileUtils;
import org.junit.AfterClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.module.RequiredPdfData;
import org.pdfsam.test.InitializeJavaFxThreadRule;
import org.sejda.model.pdf.PdfMetadataFields;

/**
 * @author Andrea Vacondio
 *
 */
public class SAMBoxPdfLoadServiceTest {
    private SAMBoxPdfLoadService victim = new SAMBoxPdfLoadService(
            Arrays.asList(new PdfLoader[] { new DefaultSAMBoxLoader(), new BookmarksLevelSAMBoxLoader() }));
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeJavaFxThreadRule initJavaFxThread = new InitializeJavaFxThreadRule();

    @AfterClass
    public static void afterClass() {
        eventStudio().clear();
    }

    @Test
    public void load() throws IOException, InterruptedException, TimeoutException {
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
        waitOrTimeout(() -> PdfDescriptorLoadingStatus.LOADED == descriptor.loadingStatus().getValue(),
                timeout(seconds(2)));
        assertEquals(2, item.pages().getValue().intValue());
        assertEquals("Me", item.getInformation(PdfMetadataFields.AUTHOR));
        assertEquals("test", item.getInformation(PdfMetadataFields.KEYWORDS));
    }

    @Test
    public void invalidPdf() throws IOException, InterruptedException, TimeoutException {
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
        waitOrTimeout(() -> PdfDescriptorLoadingStatus.WITH_ERRORS == descriptor.loadingStatus().getValue(),
                timeout(seconds(2)));
    }

    @Test
    public void encNoPwd() throws IOException, InterruptedException, TimeoutException {
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
        waitOrTimeout(() -> PdfDescriptorLoadingStatus.ENCRYPTED == descriptor.loadingStatus().getValue(),
                timeout(seconds(2)));
    }

    @Test
    public void encWithPwd() throws IOException, InterruptedException, TimeoutException {
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
        waitOrTimeout(() -> PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION == descriptor.loadingStatus()
                .getValue(), timeout(seconds(2)));
    }

    @Test
    public void invalidate() throws IOException, InterruptedException, TimeoutException {
        File testFile = folder.newFile("PDFsamTest.pdf");
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/test_pdfsam.pdf"), testFile);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(testFile);
        List<PdfDocumentDescriptor> toLoad = Arrays.asList(new PdfDocumentDescriptor[] { descriptor });
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        descriptor.releaseAll();
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        waitOrTimeout(() -> PdfDescriptorLoadingStatus.REQUESTED == descriptor.loadingStatus().getValue(),
                timeout(seconds(2)));
    }
}
