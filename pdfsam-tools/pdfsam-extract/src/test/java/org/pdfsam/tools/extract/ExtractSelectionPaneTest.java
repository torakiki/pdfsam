/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28 ago 2016
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
package org.pdfsam.tools.extract;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadInitializeExtension;
import org.sejda.conversion.exception.ConversionException;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Consumer;

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
public class ExtractSelectionPaneTest {

    private static final String TOOL = "TOOLD";

    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(TOOL);
    @TempDir
    private Path folder;
    private ExtractParametersBuilder builder;
    private Consumer<String> onError;
    private ExtractSelectionPane victim;

    @BeforeEach
    public void setUp() {
        builder = mock(ExtractParametersBuilder.class);
        onError = mock(Consumer.class);
        victim = new ExtractSelectionPane(TOOL);
    }

    @Test
    public void empty() {
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).addSource(any());
    }

    @Test
    public void conversionException() throws Exception {
        populate();
        doThrow(new ConversionException("message")).when(builder).addSource(any());
        victim.apply(builder, onError);
        verify(builder).addSource(any());
        verify(onError).accept(eq("message"));
    }

    @Test
    public void hasInput() throws Exception {
        populate();
        when(builder.hasInput()).thenReturn(Boolean.TRUE);
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
    }

    private void populate() throws Exception {
        File file = Files.createTempFile(folder, null, "temp.pdf").toFile();
        var loadEvent = new PdfLoadRequest(TOOL);
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file));
        eventStudio().broadcast(loadEvent, TOOL);
    }
}
