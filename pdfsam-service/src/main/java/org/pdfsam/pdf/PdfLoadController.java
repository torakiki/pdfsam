/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/giu/2013
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
package org.pdfsam.pdf;

import static java.util.Objects.nonNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.inject.Inject;

import org.pdfsam.ShutdownEvent;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.Module;
import org.pdfsam.module.RequiredPdfData;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.injector.Auto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component listening for {@link PdfLoadRequestEvent}, triggering the actual pdf load and sending out a response with the result of the loading
 * 
 * @author Andrea Vacondio
 * 
 */
@Auto
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
        LOG.trace("PDF load request received");
        event.getDocuments().forEach(i -> i.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED));
        executor.execute(() -> loadService.load(event.getDocuments(), requiredLoadData.get(event.getOwnerModule())));
    }

    /**
     * Request to load a text/csv file containing a list of PDF
     * 
     * @param event
     */
    @EventListener
    public void request(PdfFilesListLoadRequest event) {
        LOG.trace("PDF load from list request received");
        if (nonNull(event.list)) {
            executor.execute(() -> {
                try {
                    PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(event.getOwnerModule());
                    new PdfListParser().apply(event.list).stream().map(PdfDocumentDescriptor::newDescriptorNoPassword)
                            .forEach(loadEvent::add);
                    if (loadEvent.getDocuments().isEmpty()) {
                        LOG.error(DefaultI18nContext.getInstance()
                                .i18n("Unable to find any valid PDF file in the list: {0}", event.list.toString()));
                    } else {
                        eventStudio().broadcast(loadEvent, event.getOwnerModule());
                    }
                } catch (Exception e) {
                    LOG.error(DefaultI18nContext.getInstance().i18n("Unable to load PDF list file from {0}",
                            event.list.toString()), e);
                }
            });
        }
    }

    @EventListener
    public void onShutdown(ShutdownEvent event) {
        executor.shutdownNow();
    }
}
