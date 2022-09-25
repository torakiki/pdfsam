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
package org.pdfsam.service.ui;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.ui.workspace.LoadWorkspaceRequest;
import org.pdfsam.model.ui.workspace.LoadWorkspaceResponse;
import org.pdfsam.model.ui.workspace.SaveWorkspaceRequest;
import org.pdfsam.model.ui.workspace.WorkspaceLoadedEvent;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
public class WorkspaceControllerTest {

    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(DefaultPriorityTestTool.ID);

    private WorkspaceController victim;
    private WorkspaceService service;
    private RecentWorkspacesService recentWorkspaces;
    private File file;

    @BeforeEach
    public void setUp() {
        file = mock(File.class);
        service = mock(WorkspaceService.class);
        recentWorkspaces = mock(RecentWorkspacesService.class);
        victim = new WorkspaceController(List.of(new DefaultPriorityTestTool()), service, recentWorkspaces);
    }

    @Test
    public void saveWorkspace() {
        Listener<SaveWorkspaceRequest> listener = mock(Listener.class);
        eventStudio().add(SaveWorkspaceRequest.class, listener, DefaultPriorityTestTool.ID);
        victim.saveWorkspace(new SaveWorkspaceRequest(file, true));
        verify(listener).onEvent(any());
        verify(service).saveWorkspace(anyMap(), eq(file));
    }

    @Test
    public void saveWorkspaceWithException() {
        Listener<SaveWorkspaceRequest> listener = mock(Listener.class);
        eventStudio().add(SaveWorkspaceRequest.class, listener, DefaultPriorityTestTool.ID);
        SaveWorkspaceRequest event = new SaveWorkspaceRequest(file, true);
        doThrow(new RuntimeException("mock")).when(listener).onEvent(event);
        victim.saveWorkspace(event);
    }

    @Test
    public void loadEmptyWorkspace() throws InterruptedException, ExecutionException {
        Listener<LoadWorkspaceRequest> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceRequest.class, listener, DefaultPriorityTestTool.ID);
        when(service.loadWorkspace(any())).thenReturn(Collections.emptyMap());
        victim.loadWorspace(new LoadWorkspaceRequest(file)).get();
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void loadWorkspaceWithException() throws InterruptedException, ExecutionException {
        Listener<LoadWorkspaceRequest> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceRequest.class, listener, DefaultPriorityTestTool.ID);
        when(service.loadWorkspace(eq(file))).thenThrow(new RuntimeException("mock"));
        Assertions.assertThrows(ExecutionException.class,
                () -> victim.loadWorspace(new LoadWorkspaceRequest(file)).get());
    }

    @Test
    public void loadWorkspace() throws InterruptedException, ExecutionException {
        Listener<LoadWorkspaceResponse> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceResponse.class, listener, DefaultPriorityTestTool.ID);
        Listener<WorkspaceLoadedEvent> loadedListener = mock(Listener.class);
        eventStudio().add(WorkspaceLoadedEvent.class, loadedListener);
        Map<String, Map<String, String>> data = new HashMap<>();
        Map<String, String> moduleData = new HashMap<>();
        moduleData.put("key", "value");
        data.put("module", moduleData);
        when(service.loadWorkspace(any())).thenReturn(data);
        CompletableFuture<Void> future = victim.loadWorspace(new LoadWorkspaceRequest(file));
        future.get();
        verify(listener).onEvent(any());
        verify(recentWorkspaces).addWorkspaceLastUsed(file);
        verify(loadedListener).onEvent(any());
    }

    @Test
    public void loadWorkspaceNoDataForModule() throws InterruptedException, ExecutionException {
        Listener<LoadWorkspaceRequest> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceRequest.class, listener, "anotherModule");
        var moduleData = Map.of("key", "value");
        var data = Map.of("anotherModule", moduleData);
        when(service.loadWorkspace(any())).thenReturn(data);
        victim.loadWorspace(new LoadWorkspaceRequest(file)).get();
        verify(listener, never()).onEvent(any());
        verify(recentWorkspaces).addWorkspaceLastUsed(any());
    }

}
