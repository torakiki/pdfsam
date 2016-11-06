/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/giu/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.pdf;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.ShutdownEvent;
import org.pdfsam.module.Module;
import org.pdfsam.module.RequiredPdfData;
import org.sejda.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component listening for {@link PdfLoadRequestEvent}, triggering the actual pdf load and sending out a response with the result of the loading
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class PdfLoadController {

    private static final Logger LOG = LoggerFactory.getLogger(PdfLoadController.class);

    private PdfLoadService loadService;
    private ExecutorService executor = Executors.newSingleThreadExecutor();
    private Map<String, RequiredPdfData[]> requiredLoadData = new HashMap<>();

    @Inject
    public PdfLoadController(List<Module> modules, PdfLoadService loadService) {
        this.loadService = loadService;
        modules.forEach(m -> requiredLoadData.put(m.id(), m.requires()));
        eventStudio().addAnnotatedListeners(this);
    }

    /**
     * Request to load a collection of documents
     * 
     * @param event
     */
    @EventListener
    public void request(PdfLoadRequestEvent event) {
        LOG.trace("Pdf load request received");
        event.getDocuments().forEach(i -> i.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED));
        executor.execute(() -> loadService.load(event.getDocuments(), requiredLoadData.get(event.getOwnerModule())));
    }

    @EventListener
    public void onShutdown(ShutdownEvent event) {
        executor.shutdownNow();
    }
}
