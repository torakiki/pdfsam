package org.pdfsam.gui;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/01/23
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

import jakarta.inject.Inject;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.ui.components.tool.RunButtonTriggerRequest;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Handles the global accelerator that triggers the Run button
 *
 * @author Andrea Vacondio
 */
@Auto
public class RunAcceleratorController {

    private final ApplicationContext context;

    @Inject
    public RunAcceleratorController() {
        this(app());
    }

    public RunAcceleratorController(ApplicationContext context) {
        this.context = context;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onRunButtonAccelerator(RunButtonTriggerRequest request) {
        context.runtimeState().activeToolValue().ifPresent(m -> eventStudio().broadcast(request, m.id()));
    }
}
