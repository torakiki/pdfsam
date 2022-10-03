/*
 * This file is part of the PDF Split And Merge source code
 * Created on 31 ago 2016
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
package org.pdfsam.tools.alternatemix;

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
import org.sejda.model.input.PdfMixInput;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Consumer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(JavaFxThreadInitializeExtension.class)
public class AlternateMixSelectionPaneTest {
    private static final String TOOL = "TOOL";

    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(TOOL);
    @TempDir
    Path folder;
    private AlternateMixParametersBuilder builder;
    private Consumer<String> onError;
    private AlternateMixSelectionPane victim;

    @BeforeEach
    public void setUp() {
        builder = mock(AlternateMixParametersBuilder.class);
        onError = mock(Consumer.class);
        victim = new AlternateMixSelectionPane(TOOL);
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
        assertThat(input.getValue().getPages(10)).containsOnly(3, 4, 5, 8);
    }

    private void populate() throws Exception {
        File file = Files.createTempFile(folder, null, "temp.pdf").toFile();
        var loadEvent = new PdfLoadRequest(TOOL);
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file));
        eventStudio().broadcast(loadEvent, TOOL);
    }
}
