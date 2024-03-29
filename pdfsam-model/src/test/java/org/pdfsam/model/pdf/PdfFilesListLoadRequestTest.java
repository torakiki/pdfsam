/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30 apr 2019
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
package org.pdfsam.model.pdf;

import org.junit.jupiter.api.Test;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfFilesListLoadRequestTest {
    @Test
    public void nullFile() {
        assertThrows(IllegalArgumentException.class, () -> new PdfFilesListLoadRequest("module", null));
    }

    @Test
    public void blankModule() {
        assertThrows(IllegalArgumentException.class, () -> new PdfFilesListLoadRequest("  ", mock(Path.class)));
    }

    @Test
    public void valid() {
        new PdfFilesListLoadRequest("module", mock(Path.class));
    }
}
