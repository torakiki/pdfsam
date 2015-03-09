/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/mar/2015
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

import static org.sejda.common.ComponentsUtility.nullSafeCloseQuietly;
import static org.sejda.core.notification.dsl.ApplicationEventsNotifier.notifyEvent;

import java.io.Closeable;
import java.io.File;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.sejda.model.exception.TaskException;
import org.sejda.model.exception.TaskIOException;
import org.sejda.model.exception.TaskPermissionsException;
import org.sejda.model.input.PdfMixInput;
import org.sejda.model.input.PdfMixInput.PdfMixInputProcessStatus;
import org.sejda.model.input.PdfSourceOpener;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.encryption.PdfAccessPermission;
import org.sejda.model.task.NotifiableTaskMetadata;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component providing functionalities to perform an alternate mix on two {@link PdfMixInput}.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PdfAlternateMixer implements Closeable {

    private static final Logger LOG = LoggerFactory.getLogger(PdfAlternateMixer.class);

    private PdfMixInput firstInput;
    private PdfMixInput secondInput;
    private PDDocument fistDocument;
    private PDDocument secondDocument;
    private PdfSourceOpener<PDDocument> documentLoader = new DefaultPdfSourceOpener();
    private PDDocumentHandler destinationDocument;

    public PdfAlternateMixer(PdfMixInput firstInput, PdfMixInput secondInput) {
        this.firstInput = firstInput;
        this.secondInput = secondInput;
        destinationDocument = new PDDocumentHandler();
    }

    /**
     * Perform the alternate mix on the given {@link PdfMixInput}s.
     * 
     * @param taskMetadata
     *            metadata of the task executing the mix.
     * @throws TaskException
     */
    public void mix(NotifiableTaskMetadata taskMetadata) throws TaskException {
        fistDocument = openInput(firstInput);
        secondDocument = openInput(secondInput);

        PdfMixInputProcessStatus firstDocStatus = firstInput.newProcessingStatus(fistDocument.getNumberOfPages());
        PdfMixInputProcessStatus secondDocStatus = secondInput.newProcessingStatus(secondDocument.getNumberOfPages());

        int currentStep = 0;
        int totalSteps = fistDocument.getNumberOfPages() + secondDocument.getNumberOfPages();
        while (firstDocStatus.hasNextPage() || secondDocStatus.hasNextPage()) {
            for (int i = 0; i < firstInput.getStep() && firstDocStatus.hasNextPage(); i++) {
                destinationDocument.importPage(fistDocument.getPage(firstDocStatus.nextPage() - 1));
                notifyEvent(taskMetadata).stepsCompleted(++currentStep).outOf(totalSteps);
            }
            for (int i = 0; i < secondInput.getStep() && secondDocStatus.hasNextPage(); i++) {
                destinationDocument.importPage(secondDocument.getPage(secondDocStatus.nextPage() - 1));
                notifyEvent(taskMetadata).stepsCompleted(++currentStep).outOf(totalSteps);
            }
        }
    }

    private PDDocument openInput(PdfMixInput input) throws TaskIOException, TaskPermissionsException {
        LOG.debug("Opening input {} ", input.getSource());
        PDDocument document = input.getSource().open(documentLoader);
        new PDDocumentAccessPermission(document).ensurePermission(PdfAccessPermission.ASSEMBLE);
        return document;
    }

    public void compressXrefStream(boolean compress) {
        destinationDocument.compressXrefStream(compress);
    }

    public void setVersion(PdfVersion version) {
        destinationDocument.setVersion(version);
    }

    public void save(File file) throws TaskException {
        destinationDocument.saveDecryptedPDDocument(file);
    }

    @Override
    public void close() {
        nullSafeCloseQuietly(fistDocument);
        nullSafeCloseQuietly(secondDocument);
        nullSafeCloseQuietly(destinationDocument);
    }
}
