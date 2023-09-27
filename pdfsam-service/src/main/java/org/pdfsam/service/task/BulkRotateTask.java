/*
 * Created on 21 giu 2016
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
 * This file is part of Sejda.
 *
 * Sejda is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Sejda is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Sejda.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.service.task;

import org.pdfsam.model.task.BulkRotateParameters;
import org.pdfsam.model.task.PdfRotationInput;
import org.sejda.core.support.io.MultipleOutputWriter;
import org.sejda.core.support.io.OutputWriters;
import org.sejda.impl.sambox.component.DefaultPdfSourceOpener;
import org.sejda.impl.sambox.component.PDDocumentHandler;
import org.sejda.impl.sambox.component.PdfRotator;
import org.sejda.model.exception.TaskException;
import org.sejda.model.input.PdfSourceOpener;
import org.sejda.model.pdf.encryption.PdfAccessPermission;
import org.sejda.model.task.BaseTask;
import org.sejda.model.task.TaskExecutionContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

import static org.sejda.commons.util.IOUtils.closeQuietly;
import static org.sejda.core.notification.dsl.ApplicationEventsNotifier.notifyEvent;
import static org.sejda.core.support.io.model.FileOutput.file;
import static org.sejda.core.support.prefix.NameGenerator.nameGenerator;
import static org.sejda.core.support.prefix.model.NameGenerationRequest.nameRequest;
import static org.sejda.model.util.IOUtils.createTemporaryBuffer;

/**
 * Performs rotations on a bulk of inputs
 * 
 * @author Andrea Vacondio
 */
public class BulkRotateTask extends BaseTask<BulkRotateParameters> {

    private static final Logger LOG = LoggerFactory.getLogger(BulkRotateTask.class);

    private int totalSteps;
    private PDDocumentHandler documentHandler = null;
    private MultipleOutputWriter outputWriter;
    private PdfSourceOpener<PDDocumentHandler> documentLoader;

    @Override
    public void before(BulkRotateParameters parameters, TaskExecutionContext executionContext) throws TaskException {
        super.before(parameters, executionContext);
        totalSteps = parameters.getInputSet().size();
        documentLoader = new DefaultPdfSourceOpener();
        outputWriter = OutputWriters.newMultipleOutputWriter(parameters.getExistingOutputPolicy(), executionContext);
    }

    @Override
    public void execute(BulkRotateParameters parameters) throws TaskException {
        int currentStep = 0;

        for (PdfRotationInput input : parameters.getInputSet()) {
            currentStep++;
            LOG.debug("Opening {}", input.source);
            try {
                executionContext().notifiableTaskMetadata().setCurrentSource(input.source);
                documentHandler = input.source.open(documentLoader);
                documentHandler.getPermissions().ensurePermission(PdfAccessPermission.ASSEMBLE);
                documentHandler.setCreatorOnPDDocument();

                File tmpFile = createTemporaryBuffer(parameters.getOutput());
                LOG.debug("Created output on temporary buffer {}", tmpFile);

                PdfRotator rotator = new PdfRotator(documentHandler.getUnderlyingPDDocument());
                for (Integer page : input.getPages(documentHandler.getNumberOfPages())) {
                    rotator.rotate(page, input.rotation);
                }
                documentHandler.setVersionOnPDDocument(parameters.getVersion());
                documentHandler.setCompress(parameters.isCompress());
                documentHandler.savePDDocument(tmpFile, parameters.getOutput().getEncryptionAtRestPolicy());

                String outName = nameGenerator(parameters.getOutputPrefix())
                        .generate(nameRequest().originalName(input.source.getName()).fileNumber(currentStep));
                outputWriter.addOutput(file(tmpFile).name(outName));
            } finally {
                closeQuietly(documentHandler);
            }

            notifyEvent(executionContext().notifiableTaskMetadata()).stepsCompleted(currentStep).outOf(totalSteps);
        }
        executionContext().notifiableTaskMetadata().clearCurrentSource();

        parameters.getOutput().accept(outputWriter);
        LOG.debug("Input documents rotated and written to {}", parameters.getOutput());
    }

    @Override
    public void after() {
        closeQuietly(documentHandler);
    }
}
