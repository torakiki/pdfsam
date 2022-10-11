/*
 * This file is part of the PDF Split And Merge source code
 * Created on 11/dic/2014
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
package org.pdfsam.service.ui;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.model.lifecycle.ShutdownEvent;
import org.pdfsam.persistence.PreferencesRepository;
import org.pdfsam.test.ClearEventStudioExtension;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.pdfsam.service.ui.DefaultRecentWorkspacesService.MAX_CAPACITY;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ClearEventStudioExtension.class)
public class DefaultRecentWorkspacesServiceTest {

    private DefaultRecentWorkspacesService victim;

    @BeforeEach
    public void setUp() {
        this.victim = new DefaultRecentWorkspacesService(
                new PreferencesRepository("/test/org/pdfsam/recentworkspaces"));
    }

    @AfterEach
    public void tearDown() {
        victim.clear();
    }

    @Test
    public void maxCapacity(@TempDir Path folder) throws IOException, InterruptedException {
        populate(folder);
        assertEquals(DefaultRecentWorkspacesService.MAX_CAPACITY, victim.getRecentlyUsedWorkspaces().size());
    }

    @Test
    public void testClear(@TempDir Path folder) throws IOException {
        victim.addWorkspaceLastUsed(Files.createTempFile(folder, null, ".json").toFile());
        assertEquals(1, victim.getRecentlyUsedWorkspaces().size());
        victim.clear();
        assertEquals(0, victim.getRecentlyUsedWorkspaces().size());
    }

    @Test
    public void noDuplicate(@TempDir Path folder) throws IOException, InterruptedException {
        var file = Files.createTempFile(folder, null, ".json").toFile();
        victim.addWorkspaceLastUsed(file);
        Thread.sleep(150);
        victim.addWorkspaceLastUsed(file);
        assertEquals(1, victim.getRecentlyUsedWorkspaces().size());
    }

    @Test
    public void noDuplicateIsPushedTop(@TempDir Path folder) throws IOException, InterruptedException {
        var first = Files.createTempFile(folder, null, ".json").toFile();
        var second = Files.createTempFile(folder, null, ".json").toFile();
        victim.addWorkspaceLastUsed(first);
        Thread.sleep(150);
        victim.addWorkspaceLastUsed(second);
        assertEquals(2, victim.getRecentlyUsedWorkspaces().size());
        assertThat(victim.getRecentlyUsedWorkspaces()).containsExactly(second.getAbsolutePath(),
                first.getAbsolutePath());
        Thread.sleep(150);
        victim.addWorkspaceLastUsed(first);
        assertThat(victim.getRecentlyUsedWorkspaces()).containsExactly(first.getAbsolutePath(),
                second.getAbsolutePath());
    }

    @Test
    public void addWorkspace(@TempDir Path folder) throws IOException {
        var file = Files.createTempFile(folder, null, ".json").toFile();
        victim.addWorkspaceLastUsed(file);
        assertThat(victim.getRecentlyUsedWorkspaces()).containsExactly(file.getAbsolutePath());
    }

    @Test
    public void nullWorkspace() {
        assertThrows(IllegalArgumentException.class, () -> victim.addWorkspaceLastUsed(null));
    }

    @Test
    public void shutDownPersists(@TempDir Path folder) throws IOException, InterruptedException {
        var first = Files.createTempFile(folder, null, ".json").toFile();
        var second = Files.createTempFile(folder, null, ".json").toFile();
        victim.addWorkspaceLastUsed(first);
        Thread.sleep(150);
        victim.addWorkspaceLastUsed(second);
        victim.onShutdown(new ShutdownEvent());
        DefaultRecentWorkspacesService newVictim = new DefaultRecentWorkspacesService(
                new PreferencesRepository("/test/org/pdfsam/recentworkspaces"));
        assertThat(newVictim.getRecentlyUsedWorkspaces()).containsExactly(second.getAbsolutePath(),
                first.getAbsolutePath());
    }

    private void populate(Path folder) throws IOException, InterruptedException {
        for (int i = 0; i < MAX_CAPACITY + 1; i++) {
            victim.addWorkspaceLastUsed(Files.createTempFile(folder, null, ".json").toFile());
            // wait since we use currentMillis as key
            Thread.sleep(150);
        }
    }
}
