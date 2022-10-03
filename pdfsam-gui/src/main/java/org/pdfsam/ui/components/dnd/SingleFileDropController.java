/*
 * This file is part of the PDF Split And Merge source code
 * Created on 31 ago 2019
 * Copyright 2019 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.components.dnd;

import jakarta.inject.Inject;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.io.FileType;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.ui.dnd.SingleFileDroppedEvent;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Controller for single file drop event
 *
 * @author Andrea Vacondio
 */
@Auto
public class SingleFileDropController {

    @Inject
    public SingleFileDropController() {
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void request(SingleFileDroppedEvent event) {
        if (FileType.PDF.matches(event.file().getName()) && event.file().isFile()) {
            var loadEvent = new PdfLoadRequest(event.toolBinding());
            loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(event.file()));
            eventStudio().broadcast(loadEvent, event.toolBinding());
        }
    }

}
