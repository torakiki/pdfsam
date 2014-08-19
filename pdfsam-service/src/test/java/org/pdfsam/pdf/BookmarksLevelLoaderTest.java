/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/ago/2014
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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.sejda.impl.itext5.util.ITextUtils;

import com.itextpdf.text.pdf.PdfReader;

/**
 * @author Andrea Vacondio
 *
 */
public class BookmarksLevelLoaderTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private PdfReader reader;
    private PdfDocumentDescriptor descriptor;

    @Before
    public void setUp() throws IOException {
        File testFile = folder.newFile("PDFsamTest.pdf");
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/test_outline.pdf"), testFile);
        descriptor = mock(PdfDocumentDescriptor.class);
        reader = new PdfReader(testFile.getAbsolutePath());
    }

    @After
    public void tearDown() {
        ITextUtils.nullSafeClosePdfReader(reader);
    }

    @Test
    public void accept() {
        new BookmarksLevelLoader().accept(reader, descriptor);
        verify(descriptor).setMaxGoToActionDepth(3);
    }
}
