/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/dic/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.ui;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.pdfsam.ui.PreferencesRecentWorkspacesService.MAX_CAPACITY;
import static org.pdfsam.ui.PreferencesRecentWorkspacesService.WORKSPACES_PATH;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * @author Andrea Vacondio
 *
 */
public class PreferencesRecentWorkspacesServiceTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private PreferencesRecentWorkspacesService victim = new PreferencesRecentWorkspacesService();

    @After
    @Before
    public void clear() {
        victim.clear();
    }

    @Test
    public void maxCapacity() throws IOException, InterruptedException {
        populate();
        assertEquals(PreferencesRecentWorkspacesService.MAX_CAPACITY, victim.getRecentlyUsedWorkspaces().size());
    }

    @Test
    public void testClear() throws IOException {
        victim.addWorkspaceLastUsed(folder.newFile());
        victim.flush();
        assertEquals(1, victim.getRecentlyUsedWorkspaces().size());
        victim.clear();
        assertEquals(0, victim.getRecentlyUsedWorkspaces().size());
    }

    @Test
    public void noDuplicate() throws IOException, InterruptedException {
        File file = folder.newFile();
        victim.addWorkspaceLastUsed(file);
        Thread.sleep(150);
        victim.addWorkspaceLastUsed(file);
        assertEquals(1, victim.getRecentlyUsedWorkspaces().size());
    }

    @Test
    public void noDuplicateIsPushedTop() throws IOException, InterruptedException {
        File first = folder.newFile();
        File second = folder.newFile();
        victim.addWorkspaceLastUsed(first);
        Thread.sleep(150);
        victim.addWorkspaceLastUsed(second);
        assertEquals(2, victim.getRecentlyUsedWorkspaces().size());
        assertEquals(second.getAbsolutePath(), victim.getRecentlyUsedWorkspaces().get(0));
        victim.addWorkspaceLastUsed(first);
        assertEquals(2, victim.getRecentlyUsedWorkspaces().size());
        assertEquals(first.getAbsolutePath(), victim.getRecentlyUsedWorkspaces().get(0));
    }

    @Test
    public void flush() throws IOException, BackingStoreException {
        victim.addWorkspaceLastUsed(folder.newFile());
        victim.flush();
        assertTrue(Preferences.userRoot().node(WORKSPACES_PATH).keys().length > 0);
    }

    @Test
    public void addWorkspace() throws IOException {
        File file = folder.newFile();
        victim.addWorkspaceLastUsed(file);
        assertTrue(victim.getRecentlyUsedWorkspaces().contains(file.getAbsolutePath()));
    }

    @Test
    public void isPopulated() throws IOException, InterruptedException {
        populate();
        victim.flush();
        PreferencesRecentWorkspacesService newVictim = new PreferencesRecentWorkspacesService();
        assertEquals(5, newVictim.getRecentlyUsedWorkspaces().size());
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullWorkspace() {
        victim.addWorkspaceLastUsed(null);
    }

    @Test
    public void isSorted() throws BackingStoreException {
        Preferences node = Preferences.userRoot().node(WORKSPACES_PATH);
        node.put("2", "second");
        node.put("3", "third");
        node.put("1", "first");
        node.flush();
        PreferencesRecentWorkspacesService newVictim = new PreferencesRecentWorkspacesService();
        List<String> workspaces = newVictim.getRecentlyUsedWorkspaces();
        assertEquals(3, workspaces.size());
        assertEquals("third", workspaces.get(0));
        assertEquals("second", workspaces.get(1));
        assertEquals("first", workspaces.get(2));
    }

    @Test
    public void blankIsNotLoaded() throws BackingStoreException {
        Preferences node = Preferences.userRoot().node(WORKSPACES_PATH);
        node.put("1", EMPTY);
        node.flush();
        PreferencesRecentWorkspacesService newVictim = new PreferencesRecentWorkspacesService();
        assertEquals(0, newVictim.getRecentlyUsedWorkspaces().size());
    }

    private void populate() throws IOException, InterruptedException {
        for (int i = 0; i < MAX_CAPACITY + 1; i++) {
            victim.addWorkspaceLastUsed(folder.newFile());
            // wait since we use currentMillis as key
            Thread.sleep(150);
        }
    }
}
