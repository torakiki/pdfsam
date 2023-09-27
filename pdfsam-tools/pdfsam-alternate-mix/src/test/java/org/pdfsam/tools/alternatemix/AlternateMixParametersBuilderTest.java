/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/set/2014
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
package org.pdfsam.tools.alternatemix;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.input.PdfMixInput;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.AlternateMixMultipleInputParameters;
import org.sejda.model.pdf.PdfVersion;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 */
public class AlternateMixParametersBuilderTest {

    @Test
    public void build(@TempDir Path folder) throws IOException {
        AlternateMixParametersBuilder victim = new AlternateMixParametersBuilder();
        victim.compress(true);
        PdfMixInput first = new PdfMixInput(
                PdfFileSource.newInstanceNoPassword(Files.createTempFile(folder, null, ".pdf").toFile()), true, 2);
        victim.addInput(first);
        FileTaskOutput output = mock(FileTaskOutput.class);
        victim.output(output);
        victim.existingOutput(ExistingOutputPolicy.OVERWRITE);
        PdfMixInput second = new PdfMixInput(
                PdfFileSource.newInstanceNoPassword(Files.createTempFile(folder, null, ".pdf").toFile()), false, 4);
        victim.addInput(second);
        PdfMixInput third = new PdfMixInput(
                PdfFileSource.newInstanceNoPassword(Files.createTempFile(folder, null, ".pdf").toFile()));
        victim.addInput(third);
        victim.version(PdfVersion.VERSION_1_7);
        assertTrue(victim.hasInput());
        AlternateMixMultipleInputParameters params = victim.build();
        assertTrue(params.isCompress());
        assertEquals(ExistingOutputPolicy.OVERWRITE, params.getExistingOutputPolicy());
        assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
        assertEquals(3, params.getInputList().size());
        assertEquals(first, params.getInputList().get(0));
        assertEquals(second, params.getInputList().get(1));
        assertEquals(third, params.getInputList().get(2));
        assertTrue(params.getInputList().get(0).isReverse());
        assertFalse(params.getInputList().get(1).isReverse());
        assertFalse(params.getInputList().get(2).isReverse());
        assertEquals(2, params.getInputList().get(0).getStep());
        assertEquals(4, params.getInputList().get(1).getStep());
        assertEquals(1, params.getInputList().get(2).getStep());
        assertEquals(output, params.getOutput());
    }
}
