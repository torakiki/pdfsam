/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30 apr 2019
 * Copyright 2017 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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

import java.nio.file.Path;

import org.junit.Test;
import org.pdfsam.pdf.PdfFilesListLoadRequest;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfFilesListLoadRequestTest {
    @Test(expected = IllegalArgumentException.class)
    public void nullFile() {
        new PdfFilesListLoadRequest("module", null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void blankModule() {
        new PdfFilesListLoadRequest("  ", mock(Path.class));
    }

    @Test
    public void valid() {
        new PdfFilesListLoadRequest("module", mock(Path.class));
    }
}
