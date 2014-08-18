/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/ago/2014
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
package org.pdfsam.task;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeJavaFxThreadRule;
import org.sejda.eventstudio.Listener;
import org.sejda.model.notification.event.TaskExecutionStartedEvent;

/**
 * @author Andrea Vacondio
 *
 */
public class TaskEventBroadcasterTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    @Rule
    public InitializeJavaFxThreadRule javaFX = new InitializeJavaFxThreadRule();

    @Test
    public void onEvent() {
        TaskEventBroadcaster<TaskExecutionStartedEvent> victim = new TaskEventBroadcaster<>();
        TaskExecutionStartedEvent event = new TaskExecutionStartedEvent(null);
        Listener<TaskExecutionStartedEvent> listener = mock(Listener.class);
        eventStudio().add(TaskExecutionStartedEvent.class, listener);
        victim.onEvent(event);
        verify(listener, timeout(1000).times(1)).onEvent(event);
    }

}
