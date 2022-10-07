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
package org.pdfsam.gui.components.dnd;

import jakarta.inject.Provider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.gui.components.dialog.AddSubdirectoriesConfirmationDialog;
import org.pdfsam.model.pdf.PdfFilesListLoadRequest;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.ui.dnd.FilesDroppedEvent;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitTestListener;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
public class FilesDropControllerTest {
    private static final String TOOL = "tool";
    @RegisterExtension
    public static ClearEventStudioExtension extension = new ClearEventStudioExtension(TOOL);

    @TempDir
    public Path folder;
    private HitTestListener<PdfLoadRequest> listener;
    private HitTestListener<PdfFilesListLoadRequest> listListener;

    private FilesDropController victim;
    private AddSubdirectoriesConfirmationDialog dialog;

    @BeforeEach
    public void setUp() {
        Provider<AddSubdirectoriesConfirmationDialog> dialogProv = mock(Provider.class);
        dialog = mock(AddSubdirectoriesConfirmationDialog.class);
        when(dialogProv.get()).thenReturn(dialog);
        victim = new FilesDropController(dialogProv);
        listener = new HitTestListener<>();
        eventStudio().add(PdfLoadRequest.class, listener, TOOL);
        listListener = new HitTestListener<>();
        eventStudio().add(PdfFilesListLoadRequest.class, listListener);
    }

    @Test
    public void noPdf() throws IOException {
        victim.request(new FilesDroppedEvent(TOOL, true, List.of(Files.createTempFile(folder, null, null).toFile())));
        assertFalse(listener.isHit());
    }

    @Test
    public void noPdfSingle() throws IOException {
        victim.request(new FilesDroppedEvent(TOOL, false, List.of(Files.createTempFile(folder, null, null).toFile())));
        assertFalse(listener.isHit());
    }

    @Test
    public void directorySingle() throws IOException {
        victim.request(new FilesDroppedEvent(TOOL, false, List.of(Files.createTempDirectory(folder, null).toFile())));
        assertFalse(listener.isHit());
    }

    @Test
    public void directoryNoPdf() throws IOException {
        var dir = Files.createTempDirectory(folder, null);
        Files.createTempFile(dir, null, ".bla");
        victim.request(new FilesDroppedEvent(TOOL, true, List.of(dir.toFile())));
        assertFalse(listener.isHit());
    }

    @Test
    public void directoryYesPdf() throws IOException {
        var dir = Files.createTempDirectory(folder, null);
        Files.createTempFile(dir, null, ".pdf");
        victim.request(new FilesDroppedEvent(TOOL, true, List.of(dir.toFile())));
        assertTrue(listener.isHit());
    }

    @Test
    public void subdirectoryDialogFalseYesPdf() throws IOException {
        var dir = Files.createTempDirectory(folder, null);
        Path subdir = Files.createTempDirectory(dir, null);
        Files.createTempFile(subdir, null, ".pdf");
        when(dialog.response()).thenReturn(Boolean.FALSE);
        victim.request(new FilesDroppedEvent(TOOL, true, List.of(dir.toFile())));
        assertFalse(listener.isHit());
    }

    @Test
    public void subdirectoryDialogTrueYesPdf() throws IOException {
        var dir = Files.createTempDirectory(folder, null);
        Path subdir = Files.createTempDirectory(dir, null);
        Files.createTempFile(subdir, null, ".pdf");
        when(dialog.response()).thenReturn(Boolean.TRUE);
        victim.request(new FilesDroppedEvent(TOOL, true, List.of(dir.toFile())));
        assertTrue(listener.isHit());
    }

    @Test
    public void subdirectoryDialogTrueNoPdf() throws IOException {
        var dir = Files.createTempDirectory(folder, null);
        Path subdir = Files.createTempDirectory(dir, null);
        Files.createTempFile(subdir, null, ".bla");
        when(dialog.response()).thenReturn(Boolean.TRUE);
        victim.request(new FilesDroppedEvent(TOOL, true, List.of(dir.toFile())));
        assertFalse(listener.isHit());
    }

    @Test
    public void onePdf() throws IOException {
        victim.request(new FilesDroppedEvent(TOOL, true,
                Arrays.asList(Files.createTempFile(folder, null, null).toFile(),
                        Files.createTempFile(folder, null, ".pdf").toFile())));
        assertTrue(listener.isHit());
    }

    @Test
    public void onePdfSingle() throws IOException {
        victim.request(new FilesDroppedEvent(TOOL, false,
                Arrays.asList(Files.createTempFile(folder, null, null).toFile(),
                        Files.createTempFile(folder, null, ".pdf").toFile())));
        assertTrue(listener.isHit());
    }

    @Test
    public void oneCsv() throws IOException {
        victim.request(
                new FilesDroppedEvent(TOOL, true, List.of(Files.createTempFile(folder, null, ".csv").toFile())));
        assertTrue(listListener.isHit());
    }

    @Test
    public void oneTxt() throws IOException {
        victim.request(
                new FilesDroppedEvent(TOOL, true, List.of(Files.createTempFile(folder, null, ".txt").toFile())));
        assertTrue(listListener.isHit());
    }
}
