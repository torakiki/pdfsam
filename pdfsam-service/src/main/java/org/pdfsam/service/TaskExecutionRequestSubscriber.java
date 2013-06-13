/*
 * Created on 27/nov/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.service;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.sejda.core.notification.context.GlobalNotificationContext;
import org.sejda.model.notification.event.PercentageOfWorkDoneChangedEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.notification.event.TaskExecutionStartedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component listening for {@link TaskExecutionRequestEvent} and triggering the actual execution
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
@Singleton
class TaskExecutionRequestSubscriber {
    private static final Logger LOG = LoggerFactory.getLogger(TaskExecutionRequestSubscriber.class);

    private ExecutionService service;

    public TaskExecutionRequestSubscriber() {
        AnnotationProcessor.process(this);
        GlobalNotificationContext.getContext().addListener(PercentageOfWorkDoneChangedEvent.class,
                new TaskEventBroadcaster<PercentageOfWorkDoneChangedEvent>());
        GlobalNotificationContext.getContext().addListener(TaskExecutionFailedEvent.class,
                new TaskEventBroadcaster<TaskExecutionFailedEvent>());
        GlobalNotificationContext.getContext().addListener(TaskExecutionStartedEvent.class,
                new TaskEventBroadcaster<TaskExecutionStartedEvent>());
        GlobalNotificationContext.getContext().addListener(TaskExecutionCompletedEvent.class,
                new TaskEventBroadcaster<TaskExecutionCompletedEvent>());
    }

    /**
     * Request a task execution
     * 
     * @param event
     */
    @EventSubscriber
    public void request(TaskExecutionRequestEvent event) {
        LOG.trace("Task execution request received");
        service.submit(event.getParameters());
        LOG.trace("Task execution submitted");
    }

    @Inject
    public void setService(ExecutionService service) {
        this.service = service;
    }

}
