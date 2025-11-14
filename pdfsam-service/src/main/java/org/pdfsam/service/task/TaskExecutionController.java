/*
 * This file is part of the PDF Split And Merge source code
 * Created on 27/nov/2012
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

import jakarta.inject.Inject;
import javafx.application.Platform;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.lifecycle.ShutdownEvent;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.service.tool.UsageService;
import org.sejda.core.notification.context.GlobalNotificationContext;
import org.sejda.core.service.TaskExecutionService;
import org.sejda.model.notification.event.AbstractNotificationEvent;
import org.sejda.model.notification.event.PercentageOfWorkDoneChangedEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.notification.event.TaskExecutionStartedEvent;
import org.sejda.model.parameter.SplitByOutlineLevelParameters;
import org.sejda.model.parameter.base.TaskParameters;
import org.sejda.model.task.NotifiableTaskMetadata;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Component listening for {@link TaskExecutionRequest} and triggering the actual execution
 *
 * @author Andrea Vacondio
 */
@Auto
public class TaskExecutionController {
    private static final Logger LOG = LoggerFactory.getLogger(TaskExecutionController.class);

    private final TaskExecutionService executionService;
    private final UsageService usageService;
    private final ExecutorService executor = Executors.newSingleThreadExecutor();
    private String currentModule = StringUtils.EMPTY;

    @Inject
    public TaskExecutionController(TaskExecutionService executionService, UsageService usageService) {
        this.executionService = executionService;
        this.usageService = usageService;
        eventStudio().addAnnotatedListeners(this);
        GlobalNotificationContext.getContext()
                .addListener(TaskExecutionFailedEvent.class, new TaskEventBroadcaster<>());
        GlobalNotificationContext.getContext()
                .addListener(TaskExecutionStartedEvent.class, new TaskEventBroadcaster<>());
        GlobalNotificationContext.getContext().addListener(TaskExecutionCompletedEvent.class, e -> {
            e.getNotifiableTaskMetadata().skippedOutput().forEach(
                    f -> LOG.warn(i18n().tr("The following file already existed and was skipped: {0}", f.getName())));
            new TaskEventBroadcaster<>().onEvent(e);
        });
        GlobalNotificationContext.getContext()
                .addListener(PercentageOfWorkDoneChangedEvent.class, new TaskEventBroadcaster<>());
    }

    /**
     * Request a task execution
     *
     * @param event
     */
    @EventListener(priority = Integer.MAX_VALUE)
    public void request(TaskExecutionRequest event) {
        LOG.trace("Task execution request received");
        usageService.incrementUsageFor(event.toolId());
        currentModule = event.toolId();

        TaskParameters params = event.parameters();

        // Check if this is a hierarchical split request
        if (isHierarchicalSplitRequest(params)) {
            LOG.debug("Executing hierarchical split task");
            executor.execute(() -> executeHierarchicalSplit(params));
        } else {
            executor.execute(() -> executionService.execute(params));
        }
        LOG.trace("Task execution submitted");
    }

    private boolean isHierarchicalSplitRequest(TaskParameters params) {
        // Check if this is a hierarchical split request by checking the class name
        // We use the class name to avoid a circular dependency on the tool module
        return params.getClass().getSimpleName().equals("HierarchicalSplitByOutlineLevelParameters");
    }

    private void executeHierarchicalSplit(TaskParameters params) {
        if (!(params instanceof SplitByOutlineLevelParameters)) {
            LOG.error("Invalid parameters type for hierarchical split");
            GlobalNotificationContext.getContext()
                    .notifyListeners(new TaskExecutionFailedEvent(
                            new IllegalArgumentException("Invalid parameters type for hierarchical split"),
                            NotifiableTaskMetadata.NULL));
            return;
        }

        try {
            // Use reflection to instantiate and execute the hierarchical task
            Class<?> taskClass = Class.forName(
                    "org.pdfsam.tools.splitbybookmarks.HierarchicalSplitByBookmarksTask");
            Object task = taskClass.getDeclaredConstructor().newInstance();

            // Execute the task
            java.lang.reflect.Method executeMethod = taskClass.getMethod("execute",
                    SplitByOutlineLevelParameters.class);
            executeMethod.invoke(task, params);
        } catch (Exception e) {
            LOG.error("Failed to execute hierarchical split task", e);
            GlobalNotificationContext.getContext()
                    .notifyListeners(new TaskExecutionFailedEvent(e, NotifiableTaskMetadata.NULL));
        }
    }

    @EventListener
    public void onShutdown(ShutdownEvent event) {
        executor.shutdownNow();
    }

    class TaskEventBroadcaster<T extends AbstractNotificationEvent>
            implements org.sejda.model.notification.EventListener<T> {

        @Override
        public void onEvent(T event) {
            Platform.runLater(() -> eventStudio().broadcast(event));
            if (isNotBlank(currentModule)) {
                Platform.runLater(() -> eventStudio().broadcast(event, currentModule));
            }
        }
    }
}
