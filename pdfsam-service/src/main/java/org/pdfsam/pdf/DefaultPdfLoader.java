/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/giu/2014
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

import javax.inject.Named;

import org.pdfsam.module.RequiredPdfData;
import org.sejda.conversion.PdfVersionAdapter;

import com.itextpdf.text.pdf.PdfReader;

/**
 * Consumer taking a {@link PdfReader} and populating default field of a {@link PdfDocumentDescriptor} with data coming from the {@link PdfReader}.
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class DefaultPdfLoader implements PdfLoader {

    public void accept(PdfReader reader, PdfDocumentDescriptor descriptor) {
        descriptor.setPages(reader.getNumberOfPages());
        descriptor.setVersion(new PdfVersionAdapter(Character.toString(reader.getPdfVersion())).getEnumValue());
        descriptor.setInformationDictionary(reader.getInfo());
    }

    public RequiredPdfData key() {
        return RequiredPdfData.DEFAULT;
    }
}
