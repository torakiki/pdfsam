/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 31 gen 2017
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
package org.pdfsam.gui.components.dialog;

import jakarta.inject.Inject;
import jakarta.inject.Provider;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.sejda.model.exception.TaskNonLenientExecutionException;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static java.util.Objects.nonNull;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * 
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
public class LenientTaskExecutionDialogController {
    private static final Logger LOG = LoggerFactory.getLogger(LenientTaskExecutionDialogController.class);

    private TaskExecutionRequest latest;
    private Provider<LenientExecutionConfirmationDialog> dialog;

    @Inject
    public LenientTaskExecutionDialogController(Provider<LenientExecutionConfirmationDialog> dialog) {
        this.dialog = dialog;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener(priority = Integer.MIN_VALUE + 1)
    public void request(TaskExecutionRequest event) {
        this.latest = event;
    }

    @EventListener(priority = Integer.MAX_VALUE)
    public void failed(TaskExecutionFailedEvent event) {
        if (event.getFailingCause() instanceof TaskNonLenientExecutionException) {
            if (nonNull(latest) && dialog.get().response()) {
                latest.parameters().setLenient(true);
                eventStudio().broadcast(latest);
                LOG.info("Re-executing task in lenient mode");
            }
        }
    }

}
