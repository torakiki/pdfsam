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
import java.util.Collections;
import java.util.Set;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.task.BulkRotateParameters;
import org.pdfsam.task.PdfRotationInput;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.page.PageRange;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;

/**
 * @author Andrea Vacondio
 *
 */
public class RotateParametersBuilderTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private RotateParametersBuilder victim;

    @Before
    public void setUp() {
        victim = new RotateParametersBuilder();
        victim.compress(true);
        victim.existingOutput(ExistingOutputPolicy.OVERWRITE);
        victim.rotation(Rotation.DEGREES_180);
        victim.rotationType(PredefinedSetOfPages.ODD_PAGES);
        victim.prefix("prefix");
    }

    @Test
    public void buildDefaultSelection() throws IOException {
        FileOrDirectoryTaskOutput output = mock(FileOrDirectoryTaskOutput.class);
        victim.output(output);
        File file = folder.newFile("my.pdf");
        PdfFileSource source = PdfFileSource.newInstanceNoPassword(file);
        victim.addInput(source, null);
        victim.version(PdfVersion.VERSION_1_7);
        BulkRotateParameters params = victim.build();
        assertTrue(params.isCompress());
        assertEquals(ExistingOutputPolicy.OVERWRITE, params.getExistingOutputPolicy());
        assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
        Set<PdfRotationInput> inputs = params.getInputSet();
        assertEquals(1, inputs.size());
        PdfRotationInput input = inputs.iterator().next();
        assertEquals(Rotation.DEGREES_180, input.rotation);
        assertEquals(source, input.source);
        assertEquals(output, params.getOutput());
        assertEquals(3, input.getPages(5).size());
        assertEquals("prefix", params.getOutputPrefix());
    }

    @Test
    public void buildRanges() throws IOException {
        FileOrDirectoryTaskOutput output = mock(FileOrDirectoryTaskOutput.class);
        victim.output(output);
        File file = folder.newFile("my.pdf");
        PdfFileSource source = PdfFileSource.newInstanceNoPassword(file);
        victim.addInput(source, Collections.singleton(new PageRange(2, 5)));
        victim.version(PdfVersion.VERSION_1_7);
        BulkRotateParameters params = victim.build();
        Set<PdfRotationInput> inputs = params.getInputSet();
        assertEquals(1, inputs.size());
        PdfRotationInput input = inputs.iterator().next();
        assertEquals(Rotation.DEGREES_180, input.rotation);
        assertEquals(4, input.getPages(5).size());
    }

    @Test
    public void buildMultiple() throws IOException {
        FileOrDirectoryTaskOutput output = mock(FileOrDirectoryTaskOutput.class);
        victim.output(output);
        File file = folder.newFile("my.pdf");
        PdfFileSource source = PdfFileSource.newInstanceNoPassword(file);
        victim.addInput(source, Collections.singleton(new PageRange(2, 5)));
        victim.addInput(PdfFileSource.newInstanceNoPassword(file), Collections.emptySet());
        BulkRotateParameters params = victim.build();
        Set<PdfRotationInput> inputs = params.getInputSet();
        assertEquals(2, inputs.size());
    }
}
