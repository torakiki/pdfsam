/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12/08/2025
 * Copyright 2025 by Sober Lemur S.r.l. (info@soberlemur.com).
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
import org.pdfsam.model.ui.workspace.ConfirmSaveWorkspaceRequest;
import org.pdfsam.model.ui.workspace.SaveWorkspaceRequest;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Alessandro Parisi
 */
@Auto
public class SaveWorkspaceConfirmationDialogController {

    private final Provider<SaveWorkspaceConfirmationDialog> dialog;

    @Inject
    public SaveWorkspaceConfirmationDialogController(Provider<SaveWorkspaceConfirmationDialog> dialog) {
        this.dialog = dialog;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void request(ConfirmSaveWorkspaceRequest event) {
        if (dialog.get().response())
            eventStudio().broadcast(new SaveWorkspaceRequest(app().runtimeState().workspace().file()));
    }
}
