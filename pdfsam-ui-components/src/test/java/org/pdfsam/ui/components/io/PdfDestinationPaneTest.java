/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
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
package org.pdfsam.ui.components.io;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Mockito;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.model.ui.PdfVersionComboItem;
import org.pdfsam.model.ui.SetDestinationRequest;
import org.pdfsam.model.ui.workspace.WorkspaceData;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadInitializeExtension;
import org.sejda.model.pdf.PdfVersion;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.pdfsam.core.context.ApplicationContext.app;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadInitializeExtension.class })
public class PdfDestinationPaneTest {

    private static final String TOOL = "TOOL";
    @RegisterExtension
    public ClearEventStudioExtension clearStudio = new ClearEventStudioExtension(TOOL);
    private BrowsableDirectoryField destination;
    private PdfDestinationPane victim;

    @BeforeEach
    public void setUp() {
        destination = Mockito.spy(BrowsableDirectoryField.class);
        app().persistentSettings().set(BooleanPersistentProperty.PDF_COMPRESSION_ENABLED, true);
        app().persistentSettings().set(BooleanPersistentProperty.OVERWRITE_OUTPUT, true);
        app().persistentSettings().set(BooleanPersistentProperty.SMART_OUTPUT, false);
        victim = new PdfDestinationPane(destination, TOOL);
    }

    @Test
    public void setDestination(@TempDir Path folder) throws IOException {
        File footprint = Files.createFile(folder.resolve("test.pdf")).toFile();
        SetDestinationRequest event = SetDestinationRequest.requestDestination(footprint, TOOL);
        victim.setDestination(event);
        verify(destination).setTextFromFile(event.footprint());
    }

    @Test
    public void setFallbackDestination(@TempDir Path folder) throws IOException {
        destination.getTextField().setText("");
        app().persistentSettings().set(BooleanPersistentProperty.SMART_OUTPUT, true);
        File footprint = Files.createFile(folder.resolve("test.pdf")).toFile();
        SetDestinationRequest event = SetDestinationRequest.requestFallbackDestination(footprint, TOOL);
        victim.setDestination(event);
        verify(destination).setTextFromFile(event.footprint());
    }

    @Test
    public void dontSetFallbackDestinationIfFilled(@TempDir Path folder) throws IOException {
        destination.getTextField().setText("ChuckNorris");
        File footprint = Files.createFile(folder.resolve("test.pdf")).toFile();
        SetDestinationRequest event = SetDestinationRequest.requestDestination(footprint, TOOL);
        victim.setDestination(event);
        verify(destination).setTextFromFile(event.footprint());
    }

    @Test
    public void reset() {
        destination.getTextField().setText("ChuckNorris");
        victim.overwrite().setSelected(false);
        victim.resetView();
        assertEquals("", destination.getTextField().getText());
        assertTrue(victim.overwrite().isSelected());
        Set<PdfVersion> available = victim.getVersion().getItems().stream().map(PdfVersionComboItem::getVersion)
                .collect(Collectors.toSet());
        assertThat(available).containsOnly(PdfVersion.VERSION_1_5, PdfVersion.VERSION_1_6, PdfVersion.VERSION_1_7,
                PdfVersion.VERSION_2_0);
    }

    @Test
    public void dontSetFallbackDestinationIfNoSmartOutput(@TempDir Path folder) throws IOException {
        File footprint = Files.createFile(folder.resolve("test.pdf")).toFile();
        SetDestinationRequest event = SetDestinationRequest.requestFallbackDestination(footprint, TOOL);
        victim.setDestination(event);
        verify(destination, never()).setTextFromFile(any(File.class));
        verify(destination, never()).setTextFromFile(any(Path.class));
    }

    @Test
    public void saveState() {
        victim.overwrite().setSelected(true);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals(Boolean.TRUE.toString(), data.get("overwrite"));
        assertEquals(Boolean.TRUE.toString(), data.get("compress"));
        assertNull(data.get("discardBookmarks"));
        assertTrue(isNotEmpty(data.get("version")));
    }

    @Test
    public void restoreState() {
        WorkspaceData.ToolData data = new WorkspaceData.ToolData();
        data.setBoolean("overwrite", false);
        data.setBoolean("compress", true);
        data.setEnum("version", PdfVersion.VERSION_1_4);
        victim.restoreStateFrom(data);
        assertFalse(victim.overwrite().isSelected());
    }
}
