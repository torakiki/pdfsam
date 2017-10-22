/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/dic/2014
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
package org.pdfsam.ui.workspace;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class LoadWorkspaceEventTest {
    @Test(expected = IllegalArgumentException.class)
    public void nullRequiredFile() {
        new LoadWorkspaceEvent(null);
    }

    @Test
    public void requiredFile() {
        File file = mock(File.class);
        assertEquals(file, new LoadWorkspaceEvent(file).workspace());
    }

    public void setData() {
        Map<String, String> data = new HashMap<>();
        data.put("victiminput.size", "2");
        Map<String, Map<String, String>> moduleData = new HashMap<>();
        moduleData.put("module", data);
        File file = mock(File.class);
        LoadWorkspaceEvent victim = new LoadWorkspaceEvent(file);
        victim.setData(moduleData);
        assertFalse(victim.getData("module").isEmpty());
    }

    public void getNonExisting() {
        Map<String, String> data = new HashMap<>();
        data.put("victiminput.size", "2");
        Map<String, Map<String, String>> moduleData = new HashMap<>();
        moduleData.put("module", data);
        File file = mock(File.class);
        LoadWorkspaceEvent victim = new LoadWorkspaceEvent(file);
        victim.setData(moduleData);
        assertNotNull(victim.getData("CHUCK"));
        assertTrue(victim.getData("CHUCK").isEmpty());
    }
}
