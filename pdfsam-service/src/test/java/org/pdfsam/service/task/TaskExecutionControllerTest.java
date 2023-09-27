/*
 * This file is part of the PDF Split And Merge source code
 * Created on 18/ago/2014
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
package org.pdfsam.service.task;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.service.task.TaskExecutionController.TaskEventBroadcaster;
import org.pdfsam.service.tool.UsageService;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadInitializeExtension;
import org.sejda.core.notification.context.GlobalNotificationContext;
import org.sejda.core.service.TaskExecutionService;
import org.sejda.model.notification.event.TaskExecutionStartedEvent;
import org.sejda.model.parameter.base.AbstractParameters;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 *
 */
@ExtendWith({ ClearEventStudioExtension.class, JavaFxThreadInitializeExtension.class })
public class TaskExecutionControllerTest {

    private TaskExecutionService executionService;
    private UsageService usageService;
    private TaskExecutionController victim;

    @BeforeEach
    public void setUp() {
        executionService = mock(TaskExecutionService.class);
        usageService = mock(UsageService.class);
        victim = new TaskExecutionController(executionService, usageService);
    }

    @AfterAll
    public static void tearDown() {
        GlobalNotificationContext.getContext().clearListeners();
    }

    @Test
    public void request() {
        var toolId = "tool";
        AbstractParameters params = mock(AbstractParameters.class);
        victim.request(new TaskExecutionRequest(toolId, params));
        verify(usageService).incrementUsageFor(toolId);
        verify(executionService, timeout(1000).times(1)).execute(params);
    }

    @Test
    public void onEventTaskEventBroadcaster() {
        var toolId = "tool";
        AbstractParameters params = mock(AbstractParameters.class);
        victim.request(new TaskExecutionRequest(toolId, params));
        TaskEventBroadcaster<TaskExecutionStartedEvent> broadcaster = victim.new TaskEventBroadcaster<>();
        TaskExecutionStartedEvent event = new TaskExecutionStartedEvent(null);
        Listener<TaskExecutionStartedEvent> listener = mock(Listener.class);
        eventStudio().add(TaskExecutionStartedEvent.class, listener);
        Listener<TaskExecutionStartedEvent> listenerTool = mock(Listener.class);
        eventStudio().add(TaskExecutionStartedEvent.class, listenerTool, toolId);
        broadcaster.onEvent(event);
        verify(listener, timeout(1000).times(1)).onEvent(event);
        verify(listenerTool, timeout(1000).times(1)).onEvent(event);
    }

}
