/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09 ago 2016
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
package org.pdfsam.gui;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.ui.InputPdfArgumentsLoadRequest;
import org.pdfsam.test.ClearEventStudioExtension;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ClearEventStudioExtension.class })
public class InputPdfArgumentsControllerTest {

    private Listener<InputPdfArgumentsLoadRequest> listener;

    @BeforeEach
    public void setUp() {
        listener = mock(Listener.class);
        eventStudio().add(InputPdfArgumentsLoadRequest.class, listener);
    }

    @Test
    public void onNull() {
        new InputPdfArgumentsConsumer().accept(null);
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void onEmpty() {
        new InputPdfArgumentsConsumer().accept(new ArrayList<>());
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void onOtherArgs() {
        new InputPdfArgumentsConsumer().accept(Arrays.asList("-c", "--clean", "-clean"));
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void onNonExisting() {
        new InputPdfArgumentsConsumer().accept(List.of("/chuck/norris.pdf"));
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void onNonPdf(@TempDir Path folder) throws IOException {
        var files = List.of(Files.createTempFile(folder, null, ".txt").toString());
        new InputPdfArgumentsConsumer().accept(files);
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void onPdf(@TempDir Path folder) throws IOException {
        var files = List.of(Files.createTempFile(folder, null, ".pdf").toString(),
                Files.createTempFile(folder, null, ".pdf").toString());
        new InputPdfArgumentsConsumer().accept(files);
        ArgumentCaptor<InputPdfArgumentsLoadRequest> captor = ArgumentCaptor.forClass(
                InputPdfArgumentsLoadRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(2, captor.getValue().pdfs().size());
    }
}
