/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/ago/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfFilesListLoadRequest;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.tool.RequiredPdfData;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.after;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ClearEventStudioExtension.class)
public class PdfLoadControllerTest {

    private PdfLoadService loadService;
    private PdfLoadController victim;

    @BeforeEach
    public void setUp() {
        loadService = mock(PdfLoadService.class);
        victim = new PdfLoadController(Arrays.asList(new Tool[] { new DefaultPriorityTestTool() }), loadService);
    }

    @Test
    public void request() {
        PdfLoadRequest request = new PdfLoadRequest(DefaultPriorityTestTool.ID);
        PdfDocumentDescriptor first = mock(PdfDocumentDescriptor.class);
        PdfDocumentDescriptor second = mock(PdfDocumentDescriptor.class);
        request.add(first);
        request.add(second);
        victim.request(request);
        verify(first).moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        verify(second).moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        verify(loadService, timeout(1000).times(1)).load(anyCollection(), eq(RequiredPdfData.DEFAULT));
    }

    @Test
    public void emptyList(@TempDir Path folder) throws IOException {
        var list = folder.resolve("list.csv");
        List<String> lines = new ArrayList<>();
        lines.add("I don't exist");
        Files.write(list, lines);
        PdfFilesListLoadRequest event = new PdfFilesListLoadRequest(DefaultPriorityTestTool.ID, list);
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener, DefaultPriorityTestTool.ID);
        victim.request(event);
        verify(listener, after(1000).never()).onEvent(any());
    }

    @Test
    public void validList(@TempDir Path folder) throws IOException {
        var file = Files.createTempFile(folder, null, "apdf.pdf").toFile();
        var list = folder.resolve("list.csv");
        List<String> lines = new ArrayList<>();
        lines.add(file.getAbsolutePath());
        Files.write(list, lines);
        PdfFilesListLoadRequest event = new PdfFilesListLoadRequest(DefaultPriorityTestTool.ID, list);
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener, DefaultPriorityTestTool.ID);
        victim.request(event);
        ArgumentCaptor<PdfLoadRequest> captor = ArgumentCaptor.forClass(PdfLoadRequest.class);
        verify(listener, timeout(2000).times(1)).onEvent(captor.capture());
        assertEquals(1, captor.getValue().getDocuments().size());
    }
}
