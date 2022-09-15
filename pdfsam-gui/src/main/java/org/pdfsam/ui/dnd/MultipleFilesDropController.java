/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30 ago 2019
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
package org.pdfsam.ui.dnd;

import static java.util.Arrays.stream;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.inject.Provider;

import org.pdfsam.i18n.I18nContext;
import org.pdfsam.injector.Auto;
import org.pdfsam.pdf.MultipleFilesDroppedEvent;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfFilesListLoadRequest;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.dialog.AddSubdirectoriesConfirmationDialog;
import org.pdfsam.ui.notification.AddNotificationRequestEvent;
import org.pdfsam.ui.notification.NotificationType;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Controller responding to a files drop event
 * 
 * @author Andrea Vacondio
 */
@Auto
public class MultipleFilesDropController {

    private static final Logger LOG = LoggerFactory.getLogger(MultipleFilesDropController.class);

    private Provider<AddSubdirectoriesConfirmationDialog> dialog;

    @Inject
    public MultipleFilesDropController(Provider<AddSubdirectoriesConfirmationDialog> dialog) {
        this.dialog = dialog;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void request(MultipleFilesDroppedEvent event) {
        // not a PDF maybe a csv or txt containing the list
        if (event.files.size() == 1 && !event.files.get(0).isDirectory()
                && (FileType.TXT.matches(event.files.get(0).getName())
                        || FileType.CSV.matches(event.files.get(0).getName()))) {
            eventStudio().broadcast(new PdfFilesListLoadRequest(event.toolBinding(), event.files.get(0).toPath()));
        } else {
            final PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(event.toolBinding());
            getFiles(event.files).filter(f -> FileType.PDF.matches(f.getName()))
                    .map(PdfDocumentDescriptor::newDescriptorNoPassword).forEach(loadEvent::add);
            if (!loadEvent.getDocuments().isEmpty()) {
                eventStudio().broadcast(loadEvent, event.toolBinding());
            } else {
                eventStudio().broadcast(new AddNotificationRequestEvent(NotificationType.WARN,
                        I18nContext.getInstance()
                                .i18n("Drag and drop PDF files or directories containing PDF files"),
                        I18nContext.getInstance().i18n("No PDF found")));
            }
        }
    }

    private Stream<File> getFiles(List<File> files) {
        if (files.size() == 1 && files.get(0).isDirectory()) {
            try {
                if (stream(files.get(0).listFiles()).anyMatch(File::isDirectory) && dialog.get().response()) {
                    return Files.walk(files.get(0).toPath()).filter(Files::isRegularFile).map(Path::toFile).sorted();
                }
            } catch (IOException e) {
                LOG.warn("Unable to retrieve the list of files from " + files.get(0), e);
            }
            return stream(files.get(0).listFiles()).sorted();
        }
        return files.stream();
    }
}
