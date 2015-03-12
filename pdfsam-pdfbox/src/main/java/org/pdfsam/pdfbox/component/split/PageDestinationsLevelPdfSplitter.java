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

import org.apache.pdfbox.pdmodel.PDDocument;
import org.sejda.core.support.prefix.model.NameGenerationRequest;
import org.sejda.model.outline.OutlinePageDestinations;
import org.sejda.model.parameter.SplitByOutlineLevelParameters;
import org.sejda.model.split.NextOutputStrategy;
import org.sejda.model.split.PageDestinationsSplitPages;

/**
 * Splitter implementation to split at pages that have an outline item pointing to them.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PageDestinationsLevelPdfSplitter extends AbstractPdfSplitter<SplitByOutlineLevelParameters> {

    private PageDestinationsSplitPages splitPages;
    private OutlinePageDestinations outlineDestinations;

    /**
     * @param document
     *            the document to split.
     * @param parameters
     * @param outlineDestinations
     *            holder for the outline destinations the splitter has to split at.
     */
    public PageDestinationsLevelPdfSplitter(PDDocument document, SplitByOutlineLevelParameters parameters,
            OutlinePageDestinations outlineDestinations) {
        super(document, parameters);
        this.splitPages = new PageDestinationsSplitPages(outlineDestinations);
        this.outlineDestinations = outlineDestinations;
    }

    @Override
    NameGenerationRequest enrichNameGenerationRequest(NameGenerationRequest request) {
        return request.bookmark(outlineDestinations.getTitle(request.getPage()));
    }

    @Override
    NextOutputStrategy nextOutputStrategy() {
        return splitPages;
    }
}
