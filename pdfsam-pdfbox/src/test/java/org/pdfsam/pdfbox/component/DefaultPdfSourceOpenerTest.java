/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/mar/2015
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

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.io.IOUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.sejda.model.exception.TaskIOException;
import org.sejda.model.exception.TaskWrongPasswordException;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.input.PdfStreamSource;

/**
 * @author Andrea Vacondio
 *
 */
public class DefaultPdfSourceOpenerTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test(expected = TaskWrongPasswordException.class)
    public void encryptedStreamWrongPwd() throws TaskIOException {
        InputStream stream = getClass().getClassLoader().getResourceAsStream("pdf/enc_usr_own_same_pwd.pdf");
        new DefaultPdfSourceOpener().open(PdfStreamSource.newInstanceNoPassword(stream, "enc_own_pwd.pdf"));
    }

    @Test(expected = TaskIOException.class)
    public void streamWrongPdf() throws TaskIOException {
        InputStream stream = getClass().getClassLoader().getResourceAsStream("pdf/not_a_pdf.pdf");
        new DefaultPdfSourceOpener().open(PdfStreamSource.newInstanceNoPassword(stream, "not_a_pdf.pdf"));
    }

    @Test
    public void encruptedStreamRightPwd() throws TaskIOException {
        InputStream stream = getClass().getClassLoader().getResourceAsStream("pdf/enc_usr_own_same_pwd.pdf");
        assertNotNull(new DefaultPdfSourceOpener().open(PdfStreamSource.newInstanceWithPassword(stream,
                "enc_own_pwd.pdf", "test")));
    }

    @Test(expected = TaskWrongPasswordException.class)
    public void encryptedFileWrongPwd() throws TaskIOException, IOException {
        File file = folder.newFile();
        try (OutputStream output = new FileOutputStream(file)) {
            IOUtils.copy(getClass().getClassLoader().getResourceAsStream("pdf/enc_usr_own_same_pwd.pdf"), output);
            new DefaultPdfSourceOpener().open(PdfFileSource.newInstanceNoPassword(file));
        }
    }

    @Test(expected = TaskIOException.class)
    public void fileWrongPdf() throws TaskIOException, FileNotFoundException, IOException {
        File file = folder.newFile();
        try (OutputStream output = new FileOutputStream(file)) {
            IOUtils.copy(getClass().getClassLoader().getResourceAsStream("pdf/not_a_pdf.pdf"), output);
            new DefaultPdfSourceOpener().open(PdfFileSource.newInstanceNoPassword(file));
        }
    }

    @Test
    public void encryptedFileRightPwd() throws IOException, TaskIOException {
        File file = folder.newFile();
        try (OutputStream output = new FileOutputStream(file)) {
            IOUtils.copy(getClass().getClassLoader().getResourceAsStream("pdf/enc_usr_own_same_pwd.pdf"), output);
            new DefaultPdfSourceOpener().open(PdfFileSource.newInstanceWithPassword(file, "test"));
        }
    }
}
