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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.module.Module;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.pdfsam.ui.workspace.WorkspaceLoadedEvent;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
public class WorkspaceControllerTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    private WorkspaceController victim;
    private WorkspaceService service;
    private RecentWorkspacesService recentWorkspaces;
    private File file;

    @Before
    public void setUp() {
        file = mock(File.class);
        Map<String, Module> modulesMap = new HashMap<>();
        Module module = mock(Module.class);
        when(module.id()).thenReturn("module");
        modulesMap.put("module", module);
        service = mock(WorkspaceService.class);
        recentWorkspaces = mock(RecentWorkspacesService.class);
        victim = new WorkspaceController(modulesMap, service, recentWorkspaces);
    }

    @Test
    public void saveWorkspace() throws InterruptedException, ExecutionException {
        Listener<SaveWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(SaveWorkspaceEvent.class, listener, "module");
        CompletableFuture<Void> future = victim.saveWorkspace(new SaveWorkspaceEvent(file));
        future.get();
        verify(listener).onEvent(any());
        verify(service).saveWorkspace(anyMap(), eq(file));
    }

    @Test(expected = ExecutionException.class)
    public void saveWorkspaceWithException() throws InterruptedException, ExecutionException {
        Listener<SaveWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(SaveWorkspaceEvent.class, listener, "module");
        SaveWorkspaceEvent event = new SaveWorkspaceEvent(file);
        doThrow(new RuntimeException("mock")).when(listener).onEvent(event);
        CompletableFuture<Void> future = victim.saveWorkspace(event);
        future.get();
    }

    @Test
    public void loadEmptyWorkspace() throws InterruptedException, ExecutionException {
        Listener<LoadWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceEvent.class, listener, "module");
        when(service.loadWorkspace(any())).thenReturn(Collections.emptyMap());
        CompletableFuture<Void> future = victim.loadWorspace(new LoadWorkspaceEvent(file));
        future.get();
        verify(listener, never()).onEvent(any());
    }

    @Test(expected = ExecutionException.class)
    public void loadWorkspaceWithException() throws InterruptedException, ExecutionException {
        Listener<LoadWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceEvent.class, listener, "module");
        when(service.loadWorkspace(eq(file))).thenThrow(new RuntimeException("mock"));
        CompletableFuture<Void> future = victim.loadWorspace(new LoadWorkspaceEvent(file));
        future.get();
    }

    @Test
    public void loadWorkspace() throws InterruptedException, ExecutionException {
        Listener<LoadWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceEvent.class, listener, "module");
        Listener<WorkspaceLoadedEvent> loadedListener = mock(Listener.class);
        eventStudio().add(WorkspaceLoadedEvent.class, loadedListener);
        Map<String, Map<String, String>> data = new HashMap<>();
        Map<String, String> moduleData = new HashMap<>();
        moduleData.put("key", "value");
        data.put("module", moduleData);
        when(service.loadWorkspace(any())).thenReturn(data);
        CompletableFuture<Void> future = victim.loadWorspace(new LoadWorkspaceEvent(file));
        future.get();
        verify(listener).onEvent(any());
        verify(recentWorkspaces).addWorkspaceLastUsed(file);
        verify(loadedListener).onEvent(any());
    }

    @Test
    public void loadWorkspaceNoDataForModule() throws InterruptedException, ExecutionException {
        Listener<LoadWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceEvent.class, listener, "anotherModule");
        Map<String, Map<String, String>> data = new HashMap<>();
        Map<String, String> moduleData = new HashMap<>();
        moduleData.put("key", "value");
        data.put("anotherModule", moduleData);
        when(service.loadWorkspace(any())).thenReturn(data);
        CompletableFuture<Void> future = victim.loadWorspace(new LoadWorkspaceEvent(file));
        future.get();
        verify(listener, never()).onEvent(any());
        verify(recentWorkspaces).addWorkspaceLastUsed(any());
    }

}
