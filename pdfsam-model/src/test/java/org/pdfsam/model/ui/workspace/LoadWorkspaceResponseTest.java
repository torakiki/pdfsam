package org.pdfsam.model.ui.workspace;

import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/09/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@soberlemur.com).
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
class LoadWorkspaceResponseTest {

    @Test
    public void nullDataConstructor() {
        File file = mock(File.class);
        LoadWorkspaceResponse victim = new LoadWorkspaceResponse(file, null);
        assertNotNull(victim.getData("CHUCK"));
        assertTrue(victim.getData("CHUCK").isEmpty());
    }

    @Test
    public void withData() {
        Map<String, String> data = new HashMap<>();
        data.put("victiminput.size", "2");
        Map<String, Map<String, String>> moduleData = new HashMap<>();
        moduleData.put("module", data);
        File file = mock(File.class);
        LoadWorkspaceResponse victim = new LoadWorkspaceResponse(file, moduleData);
        assertFalse(victim.getData("module").isEmpty());
    }

    @Test
    public void getNonExisting() {
        Map<String, String> data = new HashMap<>();
        data.put("victiminput.size", "2");
        Map<String, Map<String, String>> moduleData = new HashMap<>();
        moduleData.put("module", data);
        File file = mock(File.class);
        LoadWorkspaceResponse victim = new LoadWorkspaceResponse(file, moduleData);
        assertNotNull(victim.getData("CHUCK"));
        assertTrue(victim.getData("CHUCK").isEmpty());
    }
}