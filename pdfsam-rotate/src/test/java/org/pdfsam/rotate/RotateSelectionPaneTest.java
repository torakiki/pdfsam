/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
package org.pdfsam.rotate;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Set;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.sejda.conversion.exception.ConversionException;

/**
 * @author Andrea Vacondio
 *
 */
public class RotateSelectionPaneTest {

    private static final String MODULE = "MODULE";

    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();

    private RotateParametersBuilder builder;
    private Consumer<String> onError;
    private RotateSelectionPane victim;

    @Before
    public void setUp() {
        builder = mock(RotateParametersBuilder.class);
        onError = mock(Consumer.class);
        victim = new RotateSelectionPane(MODULE);
    }

    @Test
    public void empty() {
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).addInput(any(), any());
    }

    @Test
    public void emptyPageSelection() throws Exception {
        populate();
        when(builder.hasInput()).thenReturn(Boolean.TRUE);
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        ArgumentCaptor<Set> ranges = ArgumentCaptor.forClass(Set.class);
        verify(builder).addInput(any(), ranges.capture());
        assertTrue(ranges.getValue().isEmpty());
    }

    @Test
    public void notEmptyPageSelection() throws Exception {
        populate();
        when(builder.hasInput()).thenReturn(Boolean.TRUE);
        victim.table().getItems().get(0).pageSelection.set("1,3-10");
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        ArgumentCaptor<Set> ranges = ArgumentCaptor.forClass(Set.class);
        verify(builder).addInput(any(), ranges.capture());
        assertEquals(2, ranges.getValue().size());
    }

    @Test
    public void converstionException() throws Exception {
        populate();
        doThrow(new ConversionException("message")).when(builder).addInput(any(), any());
        victim.apply(builder, onError);
        verify(builder).addInput(any(), any());
        verify(onError).accept(eq("message"));
    }

    @Test
    public void emptyByZeroPagesSelected() throws Exception {
        populate();
        victim.table().getItems().get(0).pageSelection.set("0");
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).addInput(any(), any());
    }

    private void populate() throws Exception {
        File file = folder.newFile("temp.pdf");
        PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(MODULE);
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file));
        eventStudio().broadcast(loadEvent, MODULE);
    }
}
