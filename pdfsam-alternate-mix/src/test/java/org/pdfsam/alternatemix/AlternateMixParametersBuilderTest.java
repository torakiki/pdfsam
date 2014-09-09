/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/set/2014
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
package org.pdfsam.alternatemix;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.io.IOException;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.AlternateMixParameters;
import org.sejda.model.pdf.PdfVersion;

/**
 * @author Andrea Vacondio
 *
 */
public class AlternateMixParametersBuilderTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void build() throws IOException {
        AlternateMixParametersBuilder victim = new AlternateMixParametersBuilder();
        victim.compress(true);
        File file = folder.newFile("my.pdf");
        PdfFileSource firstSource = PdfFileSource.newInstanceNoPassword(file);
        victim.first(firstSource);
        FileTaskOutput output = mock(FileTaskOutput.class);
        victim.output(output);
        victim.overwrite(true);
        victim.reverseFirst(true);
        victim.reverseSecond(true);
        victim.stepFirst(2);
        victim.stepSecond(4);
        PdfFileSource secondSource = PdfFileSource.newInstanceNoPassword(file);
        victim.second(secondSource);
        victim.version(PdfVersion.VERSION_1_7);
        AlternateMixParameters params = victim.build();
        assertTrue(params.isCompress());
        assertTrue(params.isOverwrite());
        assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
        assertTrue(params.getFirstInput().isReverse());
        assertTrue(params.getSecondInput().isReverse());
        assertEquals(2, params.getFirstInput().getStep());
        assertEquals(4, params.getSecondInput().getStep());
        assertEquals(output, params.getOutput());
        assertEquals(firstSource, params.getFirstInput().getSource());
        assertEquals(secondSource, params.getSecondInput().getSource());
    }
}
