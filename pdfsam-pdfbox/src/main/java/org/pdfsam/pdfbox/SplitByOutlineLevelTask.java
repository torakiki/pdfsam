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
package org.pdfsam.pdfbox;

import static org.sejda.common.ComponentsUtility.nullSafeCloseQuietly;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.pdfsam.pdfbox.component.DefaultPdfSourceOpener;
import org.pdfsam.pdfbox.component.split.PageDestinationsLevelPdfSplitter;
import org.sejda.model.exception.TaskException;
import org.sejda.model.input.PdfSourceOpener;
import org.sejda.model.outline.OutlinePageDestinations;
import org.sejda.model.parameter.SplitByOutlineLevelParameters;
import org.sejda.model.task.BaseTask;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Task splitting an input pdf document on a set of pages given by an outline level defined in the input parameter.
 * 
 * @author Andrea Vacondio
 *
 */
public class SplitByOutlineLevelTask extends BaseTask<SplitByOutlineLevelParameters> {

    private static final Logger LOG = LoggerFactory.getLogger(SplitByOutlineLevelTask.class);

    private PDDocument document = null;
    private PdfSourceOpener<PDDocument> documentLoader;
    private PageDestinationsLevelPdfSplitter splitter;

    public void before(SplitByOutlineLevelParameters parameters) {
        documentLoader = new DefaultPdfSourceOpener();
    }

    public void execute(SplitByOutlineLevelParameters parameters) throws TaskException {
        LOG.debug("Opening {} ", parameters.getSource());
        document = parameters.getSource().open(documentLoader);

        LOG.debug("Retrieving outline information for level {}", parameters.getLevelToSplitAt());
        OutlinePageDestinations pagesDestination = new PDFBoxOutlineLevelsHandler(document,
                parameters.getMatchingTitleRegEx()).getPageDestinationsForLevel(parameters.getLevelToSplitAt());
        splitter = new PageDestinationsLevelPdfSplitter(document, parameters, pagesDestination);
        LOG.debug("Starting split by outline level for {} ", parameters);
        splitter.split(getNotifiableTaskMetadata());

        LOG.debug("Input documents splitted and written to {}", parameters.getOutput());
    }

    public void after() {
        nullSafeCloseQuietly(document);
        splitter = null;
    }
}
