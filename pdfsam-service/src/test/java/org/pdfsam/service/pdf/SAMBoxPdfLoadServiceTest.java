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
package org.pdfsam.service.pdf;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.tool.RequiredPdfData;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadInitializeExtension;
import org.sejda.model.pdf.PdfMetadataFields;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

import static java.time.Duration.ofSeconds;
import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ClearEventStudioExtension.class, JavaFxThreadInitializeExtension.class })
public class SAMBoxPdfLoadServiceTest {
    private final DefaultPdfLoadService victim = new DefaultPdfLoadService(
            Arrays.asList(new PdfLoader[] { new DefaultSAMBoxLoader(), new BookmarksLevelSAMBoxLoader() }));

    @Test
    public void load(@TempDir Path folder) throws IOException {
        var testFile = folder.resolve("PDFsamTest.pdf");
        Files.copy(getClass().getResourceAsStream("/test_pdfsam.pdf"), testFile);
        var descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(testFile.toFile());
        var toLoad = List.of(descriptor);
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        await().atMost(ofSeconds(2))
                .until(() -> PdfDescriptorLoadingStatus.LOADED == descriptor.loadingStatus().getValue());
        assertEquals(2, item.pages().getValue().intValue());
        assertEquals("Me", item.getInformation(PdfMetadataFields.AUTHOR));
        assertEquals("test", item.getInformation(PdfMetadataFields.KEYWORDS));
    }

    @Test
    public void invalidPdf(@TempDir Path folder) throws IOException {
        var testFile = folder.resolve("PDFsamTest.pdf");
        Files.copy(getClass().getResourceAsStream("/im_empty.pdf"), testFile);
        var descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(testFile.toFile());
        var toLoad = List.of(descriptor);
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        await().atMost(ofSeconds(2))
                .until(() -> PdfDescriptorLoadingStatus.WITH_ERRORS == descriptor.loadingStatus().getValue());
    }

    @Test
    public void encNoPwd(@TempDir Path folder) throws IOException {
        var testFile = folder.resolve("PDFsamTest.pdf");
        Files.copy(getClass().getResourceAsStream("/enc_test_pdfsam.pdf"), testFile);
        var descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(testFile.toFile());
        var toLoad = List.of(descriptor);
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        assertNotNull(toLoad.get(0));
        await().atMost(ofSeconds(2)).until(() -> {
            System.out.println(descriptor.loadingStatus().getValue());
            return PdfDescriptorLoadingStatus.ENCRYPTED == descriptor.loadingStatus().getValue();
        });
    }

    @Test
    public void encWithPwd(@TempDir Path folder) throws IOException {
        var testFile = folder.resolve("PDFsamTest.pdf");
        Files.copy(getClass().getResourceAsStream("/enc_test_pdfsam.pdf"), testFile);
        var descriptor = PdfDocumentDescriptor.newDescriptor(testFile.toFile(), "test");
        var toLoad = List.of(descriptor);
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        await().atMost(ofSeconds(2))
                .until(() -> PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION == descriptor.loadingStatus()
                        .getValue());

    }

    @Test
    public void invalidate(@TempDir Path folder) throws IOException {
        var testFile = folder.resolve("PDFsamTest.pdf");
        Files.copy(getClass().getResourceAsStream("/test_pdfsam.pdf"), testFile);
        var descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(testFile.toFile());
        var toLoad = List.of(descriptor);
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, descriptor.loadingStatus().getValue());
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        descriptor.releaseAll();
        victim.load(toLoad, RequiredPdfData.DEFAULT);
        assertEquals(1, toLoad.size());
        PdfDocumentDescriptor item = toLoad.get(0);
        assertNotNull(item);
        await().atMost(ofSeconds(2))
                .until(() -> PdfDescriptorLoadingStatus.REQUESTED == descriptor.loadingStatus().getValue());
    }
}
