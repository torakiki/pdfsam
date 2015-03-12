/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/mar/2015
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
package org.pdfsam.pdfbox.component.split;

import static org.sejda.core.notification.dsl.ApplicationEventsNotifier.notifyEvent;
import static org.sejda.core.support.io.IOUtils.createTemporaryPdfBuffer;
import static org.sejda.core.support.io.model.FileOutput.file;
import static org.sejda.core.support.prefix.NameGenerator.nameGenerator;
import static org.sejda.core.support.prefix.model.NameGenerationRequest.nameRequest;

import java.io.File;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.pdfsam.pdfbox.component.PagesExtractor;
import org.sejda.core.support.io.MultipleOutputWriter;
import org.sejda.core.support.io.OutputWriters;
import org.sejda.core.support.prefix.model.NameGenerationRequest;
import org.sejda.model.exception.TaskException;
import org.sejda.model.parameter.base.SinglePdfSourceMultipleOutputParameters;
import org.sejda.model.split.NextOutputStrategy;
import org.sejda.model.task.NotifiableTaskMetadata;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Abstract component providing a skeletal implementation of the split execution.
 * 
 * @author Andrea Vacondio
 * @param <T>
 *            the type of parameters the splitter needs to have all the information necessary to perform the split.
 */
abstract class AbstractPdfSplitter<T extends SinglePdfSourceMultipleOutputParameters> {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractPdfSplitter.class);

    private PDDocument document;
    private int totalPages;
    private T parameters;

    /**
     * Creates a new splitter that reads pages from the given document
     */
    public AbstractPdfSplitter(PDDocument document, T parameters) {
        this.document = document;
        this.totalPages = document.getNumberOfPages();
        this.parameters = parameters;
    }

    public void split(NotifiableTaskMetadata taskMetadata) throws TaskException {
        nextOutputStrategy().ensureIsValid();
        MultipleOutputWriter outputWriter = OutputWriters.newMultipleOutputWriter(parameters.isOverwrite());

        try (PagesExtractor extractor = new PagesExtractor(document)) {
            int outputDocumentsCounter = 0;
            for (int page = 1; page <= totalPages; page++) {
                if (nextOutputStrategy().isOpening(page)) {
                    LOG.debug("Starting split at page {} of the original document", page);
                    outputDocumentsCounter++;
                }
                LOG.trace("Retaining page {} of the original document", page);
                extractor.retain(page);
                notifyEvent(taskMetadata).stepsCompleted(page).outOf(totalPages);
                if (nextOutputStrategy().isClosing(page) || page == totalPages) {
                    File tmpFile = createTemporaryPdfBuffer();
                    LOG.debug("Created output temporary buffer {}", tmpFile);
                    extractor.setVersion(parameters.getVersion());
                    extractor.compressXrefStream(parameters.isCompress());
                    extractor.save(tmpFile);
                    extractor.reset();

                    String outName = nameGenerator(parameters.getOutputPrefix())
                            .generate(
                                    enrichNameGenerationRequest(nameRequest().page(page)
                                            .originalName(parameters.getSource().getName())
                                            .fileNumber(outputDocumentsCounter)));
                    outputWriter.addOutput(file(tmpFile).name(outName));

                    LOG.debug("Ending split at page {} of the original document", page);
                }
            }
        }
        parameters.getOutput().accept(outputWriter);
    }

    /**
     * @param request
     * @return the input request enriched by an splitter extending class with specific values.
     */
    abstract NameGenerationRequest enrichNameGenerationRequest(NameGenerationRequest request);

    /**
     * @return the strategy to use to know if it's time to open a new document or close the current one.
     */
    abstract NextOutputStrategy nextOutputStrategy();

}
