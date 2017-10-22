/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/dic/2014
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
package org.pdfsam.ui;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * @author Andrea Vacondio
 *
 */
public class JsonWorkspaceServiceTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private JsonWorkspaceService victim = new JsonWorkspaceService();

    @Test(expected = IllegalArgumentException.class)
    public void saveNull() {
        victim.saveWorkspace(Collections.emptyMap(), null);
    }

    @Test
    public void saveWorkspace() throws IOException {
        File file = folder.newFile();
        assertFalse(FileUtils.readLines(file).size() > 0);
        Map<String, Map<String, String>> data = new HashMap<>();
        Map<String, String> moduleData = new HashMap<>();
        moduleData.put("key", "value");
        data.put("module", moduleData);
        victim.saveWorkspace(data, file);
        assertTrue(FileUtils.readLines(file).size() > 0);
    }

    @Test(expected = RuntimeException.class)
    public void saveWorkspaceReadOnly() throws IOException {
        File file = folder.newFile();
        file.setReadOnly();
        Map<String, Map<String, String>> data = new HashMap<>();
        Map<String, String> moduleData = new HashMap<>();
        moduleData.put("key", "value");
        data.put("module", moduleData);
        victim.saveWorkspace(data, file);
    }

    @Test(expected = IllegalArgumentException.class)
    public void loadNull() {
        victim.loadWorkspace(null);
    }

    @Test
    public void loadWorkspace() throws IOException {
        File file = folder.newFile();
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/workspace.json"), file);
        Map<String, Map<String, String>> result = victim.loadWorkspace(file);
        assertFalse(result.isEmpty());
        assertNotNull(result.get("split.bybookmarks"));
        assertFalse(result.get("split.bybookmarks").isEmpty());
    }

    @Test(expected = RuntimeException.class)
    public void loadBrokenWorkspace() throws IOException {
        File file = folder.newFile();
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/broken_workspace.json"), file);
        victim.loadWorkspace(file);
    }

    @Test(expected = RuntimeException.class)
    public void cannotAccessWorkspace() throws IOException {
        File file = folder.newFile();
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/workspace.json"), file);
        if (!file.setReadable(false)) {
            throw new RuntimeException("OS does not implement read pemissions");
        }
        victim.loadWorkspace(file);
    }

    @Test(expected = RuntimeException.class)
    public void loadNotExistent() {
        File file = new File("I dont exist");
        victim.loadWorkspace(file);
    }

}
