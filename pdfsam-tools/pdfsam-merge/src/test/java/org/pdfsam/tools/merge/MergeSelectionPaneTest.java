/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/set/2014
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
package org.pdfsam.tools.merge;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadInitializeExtension;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.input.PdfMergeInput;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(JavaFxThreadInitializeExtension.class)
public class MergeSelectionPaneTest {
    private static final String TOOL = "TOOL";

    @RegisterExtension
    public ClearEventStudioExtension clear = new ClearEventStudioExtension(TOOL);
    private MergeSelectionPane victim;
    private MergeParametersBuilder builder;
    private Consumer<String> onError;
    @TempDir
    private Path folder;

    @BeforeEach
    public void setUp() {
        builder = mock(MergeParametersBuilder.class);
        onError = mock(Consumer.class);
        victim = new MergeSelectionPane(TOOL);
    }

    @Test
    public void empty() {
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).addInput(any());
    }

    @Test
    public void emptyByZeroPagesSelected() throws Exception {
        populate();
        victim.table().getItems().get(0).pageSelection.set("0");
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).addInput(any());
    }

    @Test
    public void emptyPageSelection() throws Exception {
        populate();
        when(builder.hasInput()).thenReturn(Boolean.TRUE);
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        ArgumentCaptor<PdfMergeInput> input = ArgumentCaptor.forClass(PdfMergeInput.class);
        verify(builder).addInput(input.capture());
        assertTrue(input.getValue().getPageSelection().isEmpty());
    }

    @Test
    public void notEmptyPageSelection() throws Exception {
        populate();
        when(builder.hasInput()).thenReturn(Boolean.TRUE);
        victim.table().getItems().get(0).pageSelection.set("1,3-10");
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        ArgumentCaptor<PdfMergeInput> input = ArgumentCaptor.forClass(PdfMergeInput.class);
        verify(builder).addInput(input.capture());
        assertEquals(2, input.getValue().getPageSelection().size());
    }

    @Test
    public void conversionException() throws Exception {
        populate();
        doThrow(new ConversionException("message")).when(builder).addInput(any());
        victim.apply(builder, onError);
        verify(builder).addInput(any());
        verify(onError).accept(eq("message"));
    }

    private void populate() throws Exception {
        File file = Files.createTempFile(folder, null, "temp.pdf").toFile();
        var loadEvent = new PdfLoadRequest(TOOL);
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file));
        eventStudio().broadcast(loadEvent, TOOL);
    }
}
