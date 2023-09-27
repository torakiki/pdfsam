package org.pdfsam.gui;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.ApplicationRuntimeState;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.ui.components.tool.RunButtonTriggerRequest;

import java.util.Optional;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
@ExtendWith({ ClearEventStudioExtension.class })
class RunAcceleratorControllerTest {
    private RunAcceleratorController victim;
    private ApplicationRuntimeState runtimeState;
    private ApplicationPersistentSettings persistentSettings;

    @BeforeEach
    public void setUp() {
        this.runtimeState = mock(ApplicationRuntimeState.class);
        this.persistentSettings = mock(ApplicationPersistentSettings.class);
        var context = mock(ApplicationContext.class);
        when(context.runtimeState()).thenReturn(runtimeState);
        when(context.persistentSettings()).thenReturn(persistentSettings);
        this.victim = new RunAcceleratorController(context);

    }

    @Test
    public void runRequestIsSent() {
        when(runtimeState.activeToolValue()).thenReturn(Optional.of(new DefaultPriorityTestTool()));
        Listener<RunButtonTriggerRequest> listener = mock(Listener.class);
        eventStudio().add(RunButtonTriggerRequest.class, listener, DefaultPriorityTestTool.ID);
        victim.onRunButtonAccelerator(RunButtonTriggerRequest.INSTANCE);
        verify(listener).onEvent(RunButtonTriggerRequest.INSTANCE);
    }

    @Test
    public void runRequestIfNoActiveModule() {
        when(runtimeState.activeToolValue()).thenReturn(Optional.empty());
        Listener<RunButtonTriggerRequest> listener = mock(Listener.class);
        eventStudio().add(RunButtonTriggerRequest.class, listener, DefaultPriorityTestTool.ID);
        victim.onRunButtonAccelerator(RunButtonTriggerRequest.INSTANCE);
        verify(listener, never()).onEvent(ArgumentMatchers.any());
    }
}