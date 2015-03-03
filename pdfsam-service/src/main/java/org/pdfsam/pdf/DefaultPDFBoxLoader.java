/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/feb/2015
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

import java.text.DateFormat;
import java.util.Optional;

import javax.inject.Named;

import org.apache.commons.lang3.time.FastDateFormat;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDDocumentInformation;
import org.pdfsam.module.RequiredPdfData;
import org.sejda.model.pdf.PdfMetadataKey;
import org.sejda.model.pdf.PdfVersion;

/**
 * Consumer taking a {@link PDDocument} and populating an {@link PdfDocumentDescriptor} with data coming from the info dictionary of the {@link PDDocument}.
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class DefaultPDFBoxLoader implements PdfLoader<PDDocument> {

    private static FastDateFormat FORMATTER = FastDateFormat.getDateTimeInstance(DateFormat.FULL, DateFormat.MEDIUM);

    public void accept(PDDocument document, PdfDocumentDescriptor descriptor) {
        descriptor.pages(document.getNumberOfPages());
        descriptor.setVersion(getVersion(Float.toString(document.getVersion())));
        PDDocumentInformation info = document.getDocumentInformation();
        descriptor.putInformation(PdfMetadataKey.TITLE.getKey(), info.getTitle());
        descriptor.putInformation(PdfMetadataKey.AUTHOR.getKey(), info.getAuthor());
        descriptor.putInformation(PdfMetadataKey.CREATOR.getKey(), info.getCreator());
        descriptor.putInformation(PdfMetadataKey.SUBJECT.getKey(), info.getSubject());
        descriptor.putInformation(PdfMetadataKey.KEYWORDS.getKey(), info.getKeywords());
        descriptor.putInformation("Producer", info.getProducer());
        Optional.ofNullable(info.getCreationDate()).map(FORMATTER::format)
                .ifPresent(c -> descriptor.putInformation("FormattedCreationDate", c));
    }

    public RequiredPdfData key() {
        return RequiredPdfData.DEFAULT;
    }

    private PdfVersion getVersion(String version) {
        for (PdfVersion current : PdfVersion.values()) {
            if (Double.toString(current.getVersionAsDouble()).equals(version)) {
                return current;
            }
        }
        return null;
    }
}
