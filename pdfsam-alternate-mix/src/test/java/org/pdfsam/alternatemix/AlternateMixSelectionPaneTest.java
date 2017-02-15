/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 31 ago 2016
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.alternatemix;

import static org.hamcrest.Matchers.contains;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
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
import org.sejda.model.input.PdfMixInput;

/**
 * @author Andrea Vacondio
 *
 */
public class AlternateMixSelectionPaneTest {
    private static final String MODULE = "MODULE";

    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();

    private AlternateMixParametersBuilder builder;
    private Consumer<String> onError;
    private AlternateMixSelectionPane victim;

    @Before
    public void setUp() {
        builder = mock(AlternateMixParametersBuilder.class);
        onError = mock(Consumer.class);
        victim = new AlternateMixSelectionPane(MODULE);
    }

    @Test
    public void empty() {
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).addInput(any());
    }

    @Test
    public void invalidPace() throws Exception {
        populate();
        victim.table().getItems().get(0).pace.set("Chuck");
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).addInput(any());
    }

    @Test
    public void zeroPace() throws Exception {
        populate();
        victim.table().getItems().get(0).pace.set("0");
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).addInput(any());
    }

    @Test
    public void validInput() throws Exception {
        populate();
        when(builder.hasInput()).thenReturn(Boolean.TRUE);
        victim.table().getItems().get(0).reverse.set(true);
        victim.table().getItems().get(0).pace.set("3");
        victim.table().getItems().get(0).pageSelection.set("3-5,8");
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        ArgumentCaptor<PdfMixInput> input = ArgumentCaptor.forClass(PdfMixInput.class);
        verify(builder).addInput(input.capture());
        assertEquals(3, input.getValue().getStep());
        assertTrue(input.getValue().isReverse());
        assertThat(input.getValue().getPages(10), contains(3, 4, 5, 8));
    }

    private void populate() throws Exception {
        File file = folder.newFile("temp.pdf");
        PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(MODULE);
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file));
        eventStudio().broadcast(loadEvent, MODULE);
    }
}
