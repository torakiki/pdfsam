/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/apr/2014
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
package org.pdfsam.update;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.Closeable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.annotation.PreDestroy;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.sejda.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component listening for updates related requests
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public class UpdatesController implements Closeable {
    private static final Logger LOG = LoggerFactory.getLogger(UpdatesController.class);

    private UpdateService service;
    private ExecutorService executor = Executors.newSingleThreadExecutor();

    @Inject
    UpdatesController(UpdateService service) {
        this.service = service;
        eventStudio().addAnnotatedListeners(this);
    }

    @PreDestroy
    public void close() {
        executor.shutdownNow();
    }

    @EventListener
    public void checkForUpdates(UpdateCheckRequest event) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Checking for updates"));
        executor.execute(() -> service.checkForUpdates());
    }
}
