/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/ago/2014
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

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyCollectionOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.after;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.pdfsam.module.Module;
import org.pdfsam.module.RequiredPdfData;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestModule;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfLoadControllerTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private PdfLoadService loadService;
    private PdfLoadController victim;
    @Rule
    public TemporaryFolder tmp = new TemporaryFolder();

    @Before
    public void setUp() {
        loadService = mock(PdfLoadService.class);
        victim = new PdfLoadController(Arrays.asList(new Module[] { new DefaultPriorityTestModule() }), loadService);
    }

    @Test
    public void request() {
        PdfLoadRequestEvent event = new PdfLoadRequestEvent(DefaultPriorityTestModule.ID);
        PdfDocumentDescriptor first = mock(PdfDocumentDescriptor.class);
        PdfDocumentDescriptor second = mock(PdfDocumentDescriptor.class);
        event.add(first);
        event.add(second);
        victim.request(event);
        verify(first).moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        verify(second).moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        verify(loadService, timeout(1000).times(1)).load(anyCollectionOf(PdfDocumentDescriptor.class),
                eq(RequiredPdfData.DEFAULT));
    }

    @Test
    public void emptyList() throws IOException {
        Path list = tmp.newFile().toPath();
        List<String> lines = new ArrayList<>();
        lines.add("I don't exist");
        Files.write(list, lines);
        PdfFilesListLoadRequest event = new PdfFilesListLoadRequest(DefaultPriorityTestModule.ID, list);
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener, DefaultPriorityTestModule.ID);
        victim.request(event);
        verify(listener, after(1000).never()).onEvent(any());
    }

    @Test
    public void validList() throws IOException {
        File file1 = tmp.newFile("apdf.pdf");
        Path list = tmp.newFile().toPath();
        List<String> lines = new ArrayList<>();
        lines.add(file1.getAbsolutePath());
        Files.write(list, lines);
        PdfFilesListLoadRequest event = new PdfFilesListLoadRequest(DefaultPriorityTestModule.ID, list);
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener, DefaultPriorityTestModule.ID);
        victim.request(event);
        ArgumentCaptor<PdfLoadRequestEvent> captor = ArgumentCaptor.forClass(PdfLoadRequestEvent.class);
        verify(listener, timeout(60000).times(1)).onEvent(captor.capture());
        assertEquals(1, captor.getValue().getDocuments().size());
    }
}
