/*
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mar/2015
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.service.pdf;

import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.tool.RequiredPdfData;
import org.sejda.impl.sambox.component.OutlineUtils;
import org.sejda.sambox.pdmodel.PDDocument;

/**
 * Loader populating the descriptor with bookmarks related data.
 *
 * @author Andrea Vacondio
 */
public class BookmarksLevelSAMBoxLoader implements PdfLoader<PDDocument> {

    @Override
    public void accept(PDDocument document, PdfDocumentDescriptor descriptor) {
        descriptor.setValidBookmarksLevels(OutlineUtils.getOutlineLevelsWithPageDestination(document));
    }

    @Override
    public RequiredPdfData key() {
        return RequiredPdfData.BOOMARKS;
    }

}
