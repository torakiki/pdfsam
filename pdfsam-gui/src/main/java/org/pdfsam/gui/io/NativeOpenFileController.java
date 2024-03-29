/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/set/2014
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
package org.pdfsam.gui.io;

import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.io.NativeOpenFileRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.Desktop;
import java.awt.EventQueue;
import java.io.IOException;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Controller receiving requests to open a file with the default system application
 *
 * @author Andrea Vacondio
 */
@Auto
public class NativeOpenFileController {
    private static final Logger LOG = LoggerFactory.getLogger(NativeOpenFileController.class);

    NativeOpenFileController() {
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void openPath(NativeOpenFileRequest event) {
        EventQueue.invokeLater(() -> doOpen(event));
    }

    private void doOpen(NativeOpenFileRequest event) {
        if (Desktop.isDesktopSupported()) {
            try {
                Desktop.getDesktop().open(event.file());
            } catch (IOException e) {
                LOG.error(String.format("Unable to open '%s'", event.file().getAbsoluteFile()), e);
            }
        }
    }
}
