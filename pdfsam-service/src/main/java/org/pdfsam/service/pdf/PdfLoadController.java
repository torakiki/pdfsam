/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/giu/2013
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
package org.pdfsam.service.pdf;

import jakarta.inject.Inject;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.lifecycle.ShutdownEvent;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfFilesListLoadRequest;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.tool.RequiredPdfData;
import org.pdfsam.model.tool.Tool;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static java.util.Objects.nonNull;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Component listening for {@link PdfLoadRequest}, triggering the actual pdf load and sending out a response with the result of the loading
 *
 * @author Andrea Vacondio
 */
@Auto
public class PdfLoadController {

    private static final Logger LOG = LoggerFactory.getLogger(PdfLoadController.class);

    private final PdfLoadService loadService;
    private final ExecutorService executor = Executors.newSingleThreadExecutor();
    private final Map<String, RequiredPdfData[]> requiredLoadData = new HashMap<>();

    @Inject
    public PdfLoadController(PdfLoadService loadService) {
        this(app().runtimeState().tools().values(), loadService);
    }

    PdfLoadController(Collection<Tool> tools, PdfLoadService loadService) {
        this.loadService = loadService;
        tools.forEach(m -> requiredLoadData.put(m.id(), m.requires()));
        eventStudio().addAnnotatedListeners(this);
    }

    /**
     * Request to load a collection of documents
     *
     * @param event
     */
    @EventListener
    public void request(PdfLoadRequest event) {
        LOG.trace("PDF load request received");
        event.getDocuments().forEach(i -> i.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED));
        executor.execute(() -> loadService.load(event.getDocuments(), requiredLoadData.get(event.toolBinding())));
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
                    var loadEvent = new PdfLoadRequest(event.toolBinding());
                    new PdfListParser().apply(event.list).stream().map(PdfDocumentDescriptor::newDescriptorNoPassword)
                            .forEach(loadEvent::add);
                    if (loadEvent.getDocuments().isEmpty()) {
                        LOG.error(
                                i18n().tr("Unable to find any valid PDF file in the list: {0}", event.list.toString()));
                    } else {
                        eventStudio().broadcast(loadEvent, event.toolBinding());
                    }
                } catch (Exception e) {
                    LOG.error(i18n().tr("Unable to load PDF list file from {0}", event.list.toString()), e);
                }
            });
        }
    }

    @EventListener
    public void onShutdown(ShutdownEvent event) {
        executor.shutdownNow();
    }
}
