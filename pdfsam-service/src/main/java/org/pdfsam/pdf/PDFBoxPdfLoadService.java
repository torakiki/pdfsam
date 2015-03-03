/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/feb/2015
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
package org.pdfsam.pdf;

import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.ENCRYPTED;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.LOADED;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.LOADING;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.WITH_ERRORS;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;

import javafx.application.Platform;

import javax.inject.Inject;
import javax.inject.Named;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.RequiredPdfData;
import org.pdfsam.pdfbox.component.DefaultPdfSourceOpener;
import org.sejda.model.exception.TaskWrongPasswordException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Andrea Vacondio
 *
 */
@Named
class PDFBoxPdfLoadService implements PdfLoadService {
    private static final Logger LOG = LoggerFactory.getLogger(PDFBoxPdfLoadService.class);
    private final Map<RequiredPdfData, BiConsumer<PDDocument, PdfDocumentDescriptor>> consumers = new HashMap<>();

    private static final BiConsumer<PDDocument, PdfDocumentDescriptor> STARTER = (r, descriptor) -> {
        // NO OP
    };

    private static final BiConsumer<PDDocument, PdfDocumentDescriptor> FINISHER = (r, descriptor) -> {
        if (descriptor.hasPassword()) {
            fxMoveStatusTo(descriptor, LOADED_WITH_USER_PWD_DECRYPTION);
        } else {
            fxMoveStatusTo(descriptor, LOADED);
        }
    };

    @Inject
    public PDFBoxPdfLoadService(List<PdfLoader<PDDocument>> loaders) {
        loaders.forEach(l -> consumers.put(l.key(), l));
    }

    public void load(Collection<? extends PdfDocumentDescriptor> toLoad, RequiredPdfData... requires) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Loading pdf documents"));
        BiConsumer<PDDocument, PdfDocumentDescriptor> consumer = Arrays.stream(requires).map(consumers::get)
                .reduce(STARTER, (r, d) -> r.andThen(d)).andThen(FINISHER);

        for (PdfDocumentDescriptor current : toLoad) {
            if (!current.isInvalid()) {
                LOG.trace("Loading {}", current.getFileName());
                fxMoveStatusTo(current, LOADING);
                try (PDDocument document = current.toPdfFileSource().open(new DefaultPdfSourceOpener())) {
                    consumer.accept(document, current);
                } catch (TaskWrongPasswordException twpe) {
                    fxMoveStatusTo(current, ENCRYPTED);
                    LOG.warn("User password required for '{}'", current.getFileName(), twpe);
                } catch (Exception e) {
                    LOG.error("An error occured loading the document '{}'", current.getFileName(), e);
                    fxMoveStatusTo(current, WITH_ERRORS);
                }
                LOG.trace("Loading done for {} with status {}", current.getFileName(), current.loadingStatus()
                        .getValue());
            } else {
                LOG.trace("Skipping invalidated document {}", current.getFileName());
            }
        }
        LOG.debug(DefaultI18nContext.getInstance().i18n("Documents loaded"));
    }

    private static void fxMoveStatusTo(PdfDocumentDescriptor descriptor, PdfDescriptorLoadingStatus status) {
        Platform.runLater(() -> descriptor.moveStatusTo(status));
    }
}
