/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
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
package org.pdfsam.rotate;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.io.IOException;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.parameter.RotateParameters;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;

/**
 * @author Andrea Vacondio
 *
 */
public class RotateParametersBuilderTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void build() throws IOException {
        RotateParametersBuilder victim = new RotateParametersBuilder();
        victim.compress(true);
        DirectoryTaskOutput output = mock(DirectoryTaskOutput.class);
        victim.output(output);
        victim.overwrite(true);
        victim.rotation(Rotation.DEGREES_180);
        victim.rotationType(PredefinedSetOfPages.ODD_PAGES);
        victim.prefix("prefix");
        File file = folder.newFile("my.pdf");
        PdfFileSource source = PdfFileSource.newInstanceNoPassword(file);
        victim.addSource(source);
        victim.version(PdfVersion.VERSION_1_7);
        RotateParameters params = victim.build();
        assertTrue(params.isCompress());
        assertTrue(params.isOverwrite());
        assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
        assertEquals(Rotation.DEGREES_180, params.getRotation());
        assertEquals(PredefinedSetOfPages.ODD_PAGES, params.getPredefinedSetOfPages());
        assertEquals(output, params.getOutput());
        assertEquals(source, params.getSourceList().get(0));
        assertEquals("prefix", params.getOutputPrefix());
    }
}
