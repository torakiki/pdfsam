/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/ago/2014
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
package org.pdfsam.task;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.module.UsageService;
import org.pdfsam.task.TaskExecutionController.TaskEventBroadcaster;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeJavaFxThreadRule;
import org.sejda.core.notification.context.GlobalNotificationContext;
import org.sejda.core.service.TaskExecutionService;
import org.pdfsam.eventstudio.Listener;
import org.sejda.model.notification.event.TaskExecutionStartedEvent;
import org.sejda.model.parameter.base.AbstractParameters;

/**
 * @author Andrea Vacondio
 *
 */
public class TaskExecutionControllerTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    @Rule
    public InitializeJavaFxThreadRule javaFX = new InitializeJavaFxThreadRule();

    private TaskExecutionService executionService;
    private UsageService usageService;
    private TaskExecutionController victim;

    @Before
    public void setUp() {
        executionService = mock(TaskExecutionService.class);
        usageService = mock(UsageService.class);
        victim = new TaskExecutionController(executionService, usageService);
    }

    @AfterClass
    public static void tearDown() {
        GlobalNotificationContext.getContext().clearListeners();
    }

    @Test
    public void request() {
        String moduleId = "module";
        AbstractParameters params = mock(AbstractParameters.class);
        victim.request(new TaskExecutionRequestEvent(moduleId, params));
        verify(usageService).incrementUsageFor(moduleId);
        verify(executionService, timeout(1000).times(1)).execute(params);
    }

    @Test
    public void onEventTaskEventBroadcaster() {
        String moduleId = "module";
        AbstractParameters params = mock(AbstractParameters.class);
        victim.request(new TaskExecutionRequestEvent(moduleId, params));
        TaskEventBroadcaster<TaskExecutionStartedEvent> broadcaster = victim.new TaskEventBroadcaster<>();
        TaskExecutionStartedEvent event = new TaskExecutionStartedEvent(null);
        Listener<TaskExecutionStartedEvent> listener = mock(Listener.class);
        eventStudio().add(TaskExecutionStartedEvent.class, listener);
        Listener<TaskExecutionStartedEvent> listenerModule = mock(Listener.class);
        eventStudio().add(TaskExecutionStartedEvent.class, listenerModule, moduleId);
        broadcaster.onEvent(event);
        verify(listener, timeout(1000).times(1)).onEvent(event);
        verify(listenerModule, timeout(1000).times(1)).onEvent(event);
    }

}
