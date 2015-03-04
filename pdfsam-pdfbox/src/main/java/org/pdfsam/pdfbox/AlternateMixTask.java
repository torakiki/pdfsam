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
package org.pdfsam.pdfbox;

import static org.sejda.common.ComponentsUtility.nullSafeCloseQuietly;
import static org.sejda.core.support.io.IOUtils.createTemporaryPdfBuffer;
import static org.sejda.core.support.io.model.FileOutput.file;

import java.io.File;

import org.pdfsam.pdfbox.component.PdfAlternateMixer;
import org.sejda.core.support.io.OutputWriters;
import org.sejda.core.support.io.SingleOutputWriter;
import org.sejda.model.exception.TaskException;
import org.sejda.model.parameter.AlternateMixParameters;
import org.sejda.model.task.BaseTask;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * PDFBox implementation of the AlternateMix task performing the mix of two given {@link org.sejda.model.input.PdfMixInput}s.
 * 
 * @author Andrea Vacondio
 * 
 */
public class AlternateMixTask extends BaseTask<AlternateMixParameters> {

    private static final Logger LOG = LoggerFactory.getLogger(AlternateMixTask.class);

    private PdfAlternateMixer mixer = null;
    private SingleOutputWriter outputWriter;

    public void before(AlternateMixParameters parameters) {
        mixer = new PdfAlternateMixer(parameters.getFirstInput(), parameters.getSecondInput());
        outputWriter = OutputWriters.newSingleOutputWriter(parameters.isOverwrite());
    }

    public void execute(AlternateMixParameters parameters) throws TaskException {

        File tmpFile = createTemporaryPdfBuffer();
        LOG.debug("Created output temporary buffer {}", tmpFile);

        mixer.mix(getNotifiableTaskMetadata());
        mixer.setVersion(parameters.getVersion());
        mixer.compressXrefStream(parameters.isCompress());
        mixer.saveDecryptedPDDocument(tmpFile);
        nullSafeCloseQuietly(mixer);

        outputWriter.setOutput(file(tmpFile).name(parameters.getOutputName()));
        parameters.getOutput().accept(outputWriter);

        LOG.debug("Alternate mix with step first document {} and step second document {} completed.", parameters
                .getFirstInput().getStep(), parameters.getSecondInput().getStep());
    }

    public void after() {
        nullSafeCloseQuietly(mixer);
    }

}
