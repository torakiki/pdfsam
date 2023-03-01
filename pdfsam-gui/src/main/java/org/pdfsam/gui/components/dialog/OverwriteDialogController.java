/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/ott/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
import org.pdfsam.eventstudio.exception.BroadcastInterruptionException;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.sejda.model.exception.TaskOutputVisitException;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.output.TaskOutputDispatcher;
import org.sejda.model.parameter.base.AbstractParameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

import static org.apache.commons.lang3.ArrayUtils.isNotEmpty;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Controller receiving task execution requests and displaying the dialog if necessary
 *
 * @author Andrea Vacondio
 */
@Auto
public class OverwriteDialogController {
    private static final Logger LOG = LoggerFactory.getLogger(OverwriteDialogController.class);

    private final Provider<OverwriteConfirmationDialog> dialog;

    @Inject
    public OverwriteDialogController(Provider<OverwriteConfirmationDialog> dialog) {
        this.dialog = dialog;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener(priority = Integer.MIN_VALUE)
    public void request(TaskExecutionRequest event) {
        AbstractParameters params = event.parameters();
        try {
            if (params.getExistingOutputPolicy() != ExistingOutputPolicy.OVERWRITE) {
                event.parameters().getOutput().accept(new TaskOutputDispatcher() {

                    @Override
                    public void dispatch(FileOrDirectoryTaskOutput output) {
                        onDirectory(params, output.getDestination());
                    }

                    @Override
                    public void dispatch(DirectoryTaskOutput output) {
                        onDirectory(params, output.getDestination());
                    }

                    @Override
                    public void dispatch(FileTaskOutput output) {
                        onFile(params, output.getDestination());
                    }

                });
            }
        } catch (TaskOutputVisitException e) {
            // it should never happen
            LOG.warn("Unable to show overwrite confirmation dialog", e);
        }
    }

    private void onDirectory(AbstractParameters params, File dir) {
        if (isNotEmpty(dir.listFiles())) {
            OverwriteConfirmationDialog dlg = dialog.get().init();
            ExistingOutputPolicy response = dlg.title(i18n().tr("Directory not empty"))
                    .messageTitle(i18n().tr("The selected directory is not empty"))
                    .messageContent(i18n().tr("What would you like to do in case of files with the same name?"))
                    .buttons(dlg.defaultButton(i18n().tr("Overwrite"), ExistingOutputPolicy.OVERWRITE),
                            dlg.button(i18n().tr("Rename"), ExistingOutputPolicy.RENAME),
                            dlg.button(i18n().tr("Skip"), ExistingOutputPolicy.SKIP),
                            dlg.cancelButton(i18n().tr("Cancel"))).response()
                    .orElseThrow(() -> new BroadcastInterruptionException(i18n().tr("Don't overwrite existing file")));

            LOG.trace("Setting existing output policy to {}", response);
            params.setExistingOutputPolicy(response);
        }
    }

    private void onFile(AbstractParameters params, File file) {
        if (file.exists()) {
            OverwriteConfirmationDialog dlg = dialog.get().init();
            ExistingOutputPolicy response = dlg.title(i18n().tr("Overwrite confirmation"))
                    .messageTitle(i18n().tr("A file with the given name already exists"))
                    .messageContent(i18n().tr("What would you like to do?"))
                    .buttons(dlg.defaultButton(i18n().tr("Overwrite"), ExistingOutputPolicy.OVERWRITE),
                            dlg.button(i18n().tr("Rename"), ExistingOutputPolicy.RENAME),
                            dlg.cancelButton(i18n().tr("Cancel"))).response()
                    .orElseThrow(() -> new BroadcastInterruptionException(i18n().tr("Don't overwrite existing file")));
            LOG.trace("Setting existing output policy to {}", response);
            params.setExistingOutputPolicy(response);
        }
    }
}
