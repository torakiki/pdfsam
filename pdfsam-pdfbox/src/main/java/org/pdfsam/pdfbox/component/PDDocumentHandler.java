/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mar/2015
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

import static java.util.Objects.requireNonNull;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.util.GregorianCalendar;

import org.apache.pdfbox.cos.COSDictionary;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDDocumentInformation;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageTree;
import org.apache.pdfbox.pdmodel.interactive.viewerpreferences.PDViewerPreferences;
import org.sejda.core.Sejda;
import org.sejda.model.exception.TaskException;
import org.sejda.model.exception.TaskIOException;
import org.sejda.model.pdf.PdfVersion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Wrapper over a {@link PDDocument}.
 * 
 * @author Andrea Vacondio
 */
public class PDDocumentHandler implements Closeable {

    private static final Logger LOG = LoggerFactory.getLogger(PDDocumentHandler.class);

    private PDDocument document;
    private PDDocumentAccessPermission permissions;

    /**
     * Creates a new handler using the given document as underlying {@link PDDocument}.
     * 
     * @param document
     */
    public PDDocumentHandler(PDDocument document) {
        requireNonNull(document, "PDDocument cannot be null.");
        this.document = document;
        permissions = new PDDocumentAccessPermission(document);
    }

    /**
     * Creates a new handler with an empty underlying {@link PDDocument}.
     */
    public PDDocumentHandler() {
        this.document = new PDDocument();
        this.permissions = new PDDocumentAccessPermission(document);

    }


    /**
     * Set the document information on the underlying {@link PDDocument}
     * 
     * @param info
     */
    public void setDocumentInformation(PDDocumentInformation info) {
        document.setDocumentInformation(info);
    }

    /**
     * @return access permissions granted to this document.
     */
    public PDDocumentAccessPermission getPermissions() {
        return permissions;
    }

    /**
     * Sets the version on the underlying {@link PDDocument}.
     * 
     * @param version
     */
    public void setVersion(PdfVersion version) {
        if (version != null) {
            document.setVersion((float) version.getVersionAsDouble());
            LOG.trace("Version set to '{}'", version);
        }
    }

    /**
     * Set xref as a table or compressed stream on underlying {@link PDDocument}.
     * 
     * @param compress
     */
    public void compressXrefStream(boolean compress) {
        document.getDocument().setIsXRefStream(compress);
    }

    /**
     * @return the view preferences for the underlying {@link PDDocument} or an empty dictionary if no ViewerPreferences is dictionary is found.
     */
    public PDViewerPreferences getViewerPreferences() {
        PDViewerPreferences retVal = document.getDocumentCatalog().getViewerPreferences();
        if (retVal == null) {
            retVal = new PDViewerPreferences(new COSDictionary());
        }
        return retVal;
    }

    public void setViewerPreferences(PDViewerPreferences preferences) {
        document.getDocumentCatalog().setViewerPreferences(preferences);
    }

    public void close() throws IOException {
        document.close();
    }

    /**
     * Saves the underlying {@link PDDocument} removing security from it.
     * 
     * @param file
     * @throws TaskException
     */
    public void saveDecryptedPDDocument(File file) throws TaskException {
        savePDDocument(file, true);
    }

    /**
     * Saves the underlying {@link PDDocument} to the given file.
     * 
     * @param file
     * @throws TaskException
     */
    public void savePDDocument(File file) throws TaskException {
        savePDDocument(file, false);
    }

    private void savePDDocument(File file, boolean decrypted) throws TaskException {
        try {
            document.getDocumentInformation().setCreator(Sejda.CREATOR);
            document.getDocumentInformation().setProducer("PDFsam (pdfsam.org)");
            document.getDocumentInformation().setModificationDate(new GregorianCalendar());
            document.setAllSecurityToBeRemoved(decrypted);
            LOG.trace("Saving document to {}", file);
            document.save(file.getAbsolutePath());
        } catch (IOException e) {
            throw new TaskIOException("Unable to save to temporary file.", e);
        }
    }

    public int getNumberOfPages() {
        return document.getNumberOfPages();
    }

    public PDDocument getUnderlyingPDDocument() {
        return document;
    }

    /**
     * Import an existing page to the underlying {@link PDDocument}
     * 
     * @param page
     */
    public void importPage(PDPage page) {
        document.addPage(page);
    }

    public PDPage getPage(int pageNumber) {
        return document.getPage(pageNumber - 1);
    }

    public PDPageTree getPages() {
        return document.getPages();
    }

}
