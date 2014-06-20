/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/giu/2014
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
package org.pdfsam.pdf;

import java.util.function.BiConsumer;

import org.pdfsam.module.RequiredPdfData;

import com.itextpdf.text.pdf.PdfReader;

/**
 * A {@link PdfLoader} is responsible for populating a {@link PdfDocumentDescriptor} with some data read from a {@link PdfReader}.
 * 
 * @author Andrea Vacondio
 *
 */
interface PdfLoader extends BiConsumer<PdfReader, PdfDocumentDescriptor> {

    /**
     * @return the {@link RequiredPdfData} associated with this loader.
     */
    RequiredPdfData key();
}
