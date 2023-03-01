package org.pdfsam.gui.io;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 02/10/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@pdfsam.org).
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
import javafx.application.HostServices;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.io.NativeOpenUrlRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

import static java.util.Objects.nonNull;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@Auto
public class NativeOpenUrlController {
    private static final Logger LOG = LoggerFactory.getLogger(NativeOpenUrlController.class);
    private final HostServices services;

    @Inject
    public NativeOpenUrlController(HostServices services) {
        this.services = services;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void openUrl(NativeOpenUrlRequest event) {
        if (nonNull(event.url()) && !event.url().isBlank()) {
            try {
                services.showDocument(event.url());
            } catch (NullPointerException npe) {
                // service delegate can be null but there's no way to check it first so we have to catch the npe
                fallbackOpen(event);
            }
        }
    }

    private void fallbackOpen(NativeOpenUrlRequest event) {
        LOG.info("Unable to open url using HostServices, trying fallback");
        try {
            Runtime.getRuntime().exec(getOpenCmd(event.url()));
        } catch (IOException e) {
            LOG.warn("Unable to open '" + event.url() + "', please copy and paste the url to your browser", e);
        }
    }

    private static String getOpenCmd(String url) throws IOException {
        String os = System.getProperty("os.name").toLowerCase();
        if (os.contains("mac")) {
            return String.format("%s %s", "open", url);
        }
        if (os.contains("win")) {
            return String.format("%s %s", "explorer", url);
        }
        if (os.contains("nix") || os.contains("nux") || os.indexOf("aix") > 0) {
            return String.format("%s %s", "xdg-open", url);
        }
        throw new IOException("Unable to identify the open command for the OS " + os);
    }
}
