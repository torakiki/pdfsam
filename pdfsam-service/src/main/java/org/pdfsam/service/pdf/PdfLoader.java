/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/giu/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import java.util.function.BiConsumer;

/**
 * A {@link PdfLoader} is responsible for populating a {@link PdfDocumentDescriptor} with some data read from a source.
 *
 * @param <T> type of the source
 * @author Andrea Vacondio
 */
public interface PdfLoader<T> extends BiConsumer<T, PdfDocumentDescriptor> {

    /**
     * @return the {@link RequiredPdfData} associated with this loader.
     */
    RequiredPdfData key();
}
