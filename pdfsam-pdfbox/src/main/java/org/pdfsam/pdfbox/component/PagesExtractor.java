/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 06/mar/2015
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
package org.pdfsam.pdfbox.component;

import java.io.Closeable;
import java.io.File;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDDocumentOutline;
import org.sejda.common.ComponentsUtility;
import org.sejda.model.exception.TaskException;
import org.sejda.model.pdf.PdfVersion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Andrea Vacondio
 *
 */
public class PagesExtractor implements Closeable {

    private static final Logger LOG = LoggerFactory.getLogger(PagesExtractor.class);

    private OutlineMerger outlineMerger;
    private PDDocumentOutline outline = new PDDocumentOutline();
    private PDDocument originalDocument;
    private PDDocumentHandler destinationDocument = new PDDocumentHandler();

    public PagesExtractor(PDDocument origin) {
        this.outlineMerger = new OutlineMerger(origin);
        this.originalDocument = origin;
        destinationDocument.setDocumentInformation(origin.getDocumentInformation());
        destinationDocument.setViewerPreferences(origin.getDocumentCatalog().getViewerPreferences());
        destinationDocument.setPageLayout(origin.getDocumentCatalog().getPageLayout());
        destinationDocument.setPageMode(origin.getDocumentCatalog().getPageMode());
    }

    public void retain(int page) {
        PDPage existingPage = originalDocument.getPage(page - 1);
        destinationDocument.importPage(existingPage);
        outlineMerger.addRelevantPage(existingPage);
        LOG.trace("Imported page number {}", page);
    }

    /**
     * @param version
     * @see org.pdfsam.pdfbox.component.PDDocumentHandler#setVersion(org.sejda.model.pdf.PdfVersion)
     */
    public void setVersion(PdfVersion version) {
        destinationDocument.setVersion(version);
    }

    public void save(File file) throws TaskException {
        outlineMerger.mergeRelevantOutlineTo(outline);
        if (outline.hasChildren()) {
            destinationDocument.setDocumentOutline(outline);
        }
        destinationDocument.saveDecryptedPDDocument(file);
    }

    public void close() {
        ComponentsUtility.nullSafeCloseQuietly(destinationDocument);
        outlineMerger = null;
    }
}
