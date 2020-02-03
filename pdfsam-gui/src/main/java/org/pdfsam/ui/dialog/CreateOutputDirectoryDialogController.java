/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03 dic 2015
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
package org.pdfsam.ui.dialog;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;
import java.nio.file.Files;

import javax.inject.Inject;
import javax.inject.Provider;

import org.pdfsam.injector.Auto;
import org.pdfsam.ui.commons.NonExistingOutputDirectoryEvent;
import org.sejda.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Controller receiving notifications of a non existing directory and displaying the dialog to request the user which action to perform
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
public class CreateOutputDirectoryDialogController {

    private static final Logger LOG = LoggerFactory.getLogger(CreateOutputDirectoryDialogController.class);

    private Provider<CreateOutputDirectoryConfirmationDialog> dialog;

    @Inject
    public CreateOutputDirectoryDialogController(Provider<CreateOutputDirectoryConfirmationDialog> dialog) {
        this.dialog = dialog;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void request(NonExistingOutputDirectoryEvent event) {
        try {
            if (dialog.get().response()) {
                Files.createDirectories(event.outputDirectory);
                LOG.debug("Created output directory {}", event.outputDirectory);
            }
        } catch (IOException e) {
            LOG.warn("Unable to create output directory", e);
        }
    }
}
