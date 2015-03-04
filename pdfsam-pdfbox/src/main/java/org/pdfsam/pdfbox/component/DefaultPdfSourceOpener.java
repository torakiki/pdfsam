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
package org.pdfsam.pdfbox.component;

import java.io.IOException;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.encryption.InvalidPasswordException;
import org.sejda.model.exception.TaskIOException;
import org.sejda.model.exception.TaskWrongPasswordException;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.input.PdfSourceOpener;
import org.sejda.model.input.PdfStreamSource;
import org.sejda.model.input.PdfURLSource;

/**
 * PDFBox component able to open a PdfSource and return the corresponding {@link PDDocument}.
 * 
 * @author Andrea Vacondio
 * 
 */
public class DefaultPdfSourceOpener implements PdfSourceOpener<PDDocument> {

    private static final String WRONG_PWD_MESSAGE = "Unable to open the document due to a wrong password.";
    private static final String ERROR_MESSAGE = "An error occurred opening the source: %s.";

    public PDDocument open(PdfURLSource source) throws TaskIOException {
        try {
            return PDDocument.load(source.getSource().openStream(), source.getPassword());
        } catch (InvalidPasswordException ipe) {
            throw new TaskWrongPasswordException(WRONG_PWD_MESSAGE, ipe);
        } catch (IOException e) {
            throw new TaskIOException(String.format(ERROR_MESSAGE, source), e);
        }
    }

    public PDDocument open(PdfFileSource source) throws TaskIOException {
        try {
            return PDDocument.load(source.getSource(), source.getPassword());
        } catch (InvalidPasswordException ipe) {
            throw new TaskWrongPasswordException(WRONG_PWD_MESSAGE, ipe);
        } catch (IOException e) {
            throw new TaskIOException(String.format(ERROR_MESSAGE, source), e);
        }
    }

    public PDDocument open(PdfStreamSource source) throws TaskIOException {
        try {
            return PDDocument.load(source.getSource(), source.getPassword());
        } catch (InvalidPasswordException ipe) {
            throw new TaskWrongPasswordException(WRONG_PWD_MESSAGE, ipe);
        } catch (IOException e) {
            throw new TaskIOException(String.format(ERROR_MESSAGE, source), e);
        }
    }
}
