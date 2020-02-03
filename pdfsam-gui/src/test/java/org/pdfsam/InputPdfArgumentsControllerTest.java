/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09 ago 2016
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
package org.pdfsam;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.InputPdfArgumentsLoadRequest;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
public class InputPdfArgumentsControllerTest {
    @Rule
    public ClearEventStudioRule eventStudioRule = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private Listener<InputPdfArgumentsLoadRequest> listener;

    @Before
    public void setUp() {
        listener = mock(Listener.class);
        eventStudio().add(InputPdfArgumentsLoadRequest.class, listener);
    }

    @Test
    public void onNull() {
        new InputPdfArgumentsController().accept(null);
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void onEmpty() {
        new InputPdfArgumentsController().accept(new ArrayList<>());
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void onOtherArgs() {
        new InputPdfArgumentsController().accept(Arrays.asList("-c", "--clean", "-clean"));
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void onNonExisting() {
        new InputPdfArgumentsController().accept(Arrays.asList("/chuck/norris.pdf"));
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void onNonPdf() throws IOException {
        new InputPdfArgumentsController().accept(Arrays.asList(folder.newFile("some.txt").getAbsolutePath()));
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void onPdf() throws IOException {
        new InputPdfArgumentsController().accept(Arrays.asList(folder.newFile("some.pdf").getAbsolutePath(),
                folder.newFile("some_other.pdf").getAbsolutePath()));
        ArgumentCaptor<InputPdfArgumentsLoadRequest> captor = ArgumentCaptor
                .forClass(InputPdfArgumentsLoadRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(2, captor.getValue().pdfs.size());
    }
}
