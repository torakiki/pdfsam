/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/dic/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.module.Module;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
public class WorkspaceControllerTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    private WorkspaceController victim;

    @Before
    public void setUp() {
        Map<String, Module> modulesMap = new HashMap<>();
        Module module = mock(Module.class);
        when(module.id()).thenReturn("module");
        modulesMap.put("module", module);
        victim = new WorkspaceController(modulesMap);
    }

    @Test
    public void saveWorkspace() throws IOException, InterruptedException, ExecutionException {
        Listener<SaveWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(SaveWorkspaceEvent.class, listener, "module");
        File file = folder.newFile();
        assertFalse(FileUtils.readLines(file).size() > 0);
        CompletableFuture<Void> future = victim.saveWorkspace(new SaveWorkspaceEvent(file));
        future.get();
        assertFalse(future.isCompletedExceptionally());
        verify(listener).onEvent(any());
        assertTrue(FileUtils.readLines(file).size() > 0);
    }

    @Test(expected = ExecutionException.class)
    public void saveWorkspaceWithException() throws IOException, InterruptedException, ExecutionException {
        Listener<SaveWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(SaveWorkspaceEvent.class, listener, "module");
        File file = folder.newFile();
        assertFalse(FileUtils.readLines(file).size() > 0);
        SaveWorkspaceEvent event = new SaveWorkspaceEvent(file);
        doThrow(new RuntimeException("mock")).when(listener).onEvent(event);
        CompletableFuture<Void> future = victim.saveWorkspace(event);
        future.get();
    }
}
