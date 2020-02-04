/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 31 ago 2019
 * Copyright 2019 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.dnd;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

import javax.inject.Provider;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.pdf.MultipleFilesDroppedEvent;
import org.pdfsam.pdf.PdfFilesListLoadRequest;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.dialog.AddSubdirectoriesConfirmationDialog;

/**
 * @author Andrea Vacondio
 *
 */
public class MultipleFilesDropControllerTest {
    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private HitTestListener<PdfLoadRequestEvent> listener;
    private HitTestListener<PdfFilesListLoadRequest> listListener;
    private static final String MODULE = "module";
    private MultipleFilesDropController victim;
    private AddSubdirectoriesConfirmationDialog dialog;

    @Before
    public void setUp() {
        Provider<AddSubdirectoriesConfirmationDialog> dialogProv = mock(Provider.class);
        dialog = mock(AddSubdirectoriesConfirmationDialog.class);
        when(dialogProv.get()).thenReturn(dialog);
        victim = new MultipleFilesDropController(dialogProv);
        listener = new HitTestListener<>();
        eventStudio().add(PdfLoadRequestEvent.class, listener, MODULE);
        listListener = new HitTestListener<>();
        eventStudio().add(PdfFilesListLoadRequest.class, listListener);
    }

    @Test
    public void noPdf() throws IOException {
        victim.request(new MultipleFilesDroppedEvent(MODULE, Arrays.asList(folder.newFile())));
        assertFalse(listener.isHit());
    }

    @Test
    public void directoryNoPdf() throws IOException {
        File dir = folder.newFolder();
        Files.createTempFile(dir.toPath(), null, ".bla");
        victim.request(new MultipleFilesDroppedEvent(MODULE, Arrays.asList(dir)));
        assertFalse(listener.isHit());
    }

    @Test
    public void directoryYesPdf() throws IOException {
        File dir = folder.newFolder();
        Files.createTempFile(dir.toPath(), null, ".pdf");
        victim.request(new MultipleFilesDroppedEvent(MODULE, Arrays.asList(dir)));
        assertTrue(listener.isHit());
    }

    @Test
    public void subdirectoryDialogFalseYesPdf() throws IOException {
        File dir = folder.newFolder();
        Path subdir = Files.createTempDirectory(dir.toPath(), null);
        Files.createTempFile(subdir, null, ".pdf");
        when(dialog.response()).thenReturn(Boolean.FALSE);
        victim.request(new MultipleFilesDroppedEvent(MODULE, Arrays.asList(dir)));
        assertFalse(listener.isHit());
    }

    @Test
    public void subdirectoryDialogTrueYesPdf() throws IOException {
        File dir = folder.newFolder();
        Path subdir = Files.createTempDirectory(dir.toPath(), null);
        Files.createTempFile(subdir, null, ".pdf");
        when(dialog.response()).thenReturn(Boolean.TRUE);
        victim.request(new MultipleFilesDroppedEvent(MODULE, Arrays.asList(dir)));
        assertTrue(listener.isHit());
    }

    @Test
    public void subdirectoryDialogTrueNoPdf() throws IOException {
        File dir = folder.newFolder();
        Path subdir = Files.createTempDirectory(dir.toPath(), null);
        Files.createTempFile(subdir, null, ".bla");
        when(dialog.response()).thenReturn(Boolean.TRUE);
        victim.request(new MultipleFilesDroppedEvent(MODULE, Arrays.asList(dir)));
        assertFalse(listener.isHit());
    }

    @Test
    public void onePdf() throws IOException {
        victim.request(
                new MultipleFilesDroppedEvent(MODULE, Arrays.asList(folder.newFile(), folder.newFile("test.pdf"))));
        assertTrue(listener.isHit());
    }

    @Test
    public void oneCsv() throws IOException {
        victim.request(new MultipleFilesDroppedEvent(MODULE, Arrays.asList(folder.newFile("test.csv"))));
        assertTrue(listListener.isHit());
    }

    @Test
    public void oneTxt() throws IOException {
        victim.request(new MultipleFilesDroppedEvent(MODULE, Arrays.asList(folder.newFile("test.txt"))));
        assertTrue(listListener.isHit());
    }
}
