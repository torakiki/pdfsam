/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.tools.merge;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.input.PdfMergeInput;
import org.sejda.model.outline.OutlinePolicy;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.MergeParameters;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.form.AcroFormPolicy;
import org.sejda.model.toc.ToCPolicy;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 *
 */
public class MergeParametersBuilderTest {

    @Test
    public void build(@TempDir Path folder) throws IOException {
        MergeParametersBuilder victim = new MergeParametersBuilder();
        victim.compress(true);
        FileTaskOutput output = mock(FileTaskOutput.class);
        victim.output(output);
        victim.existingOutput(ExistingOutputPolicy.OVERWRITE);
        victim.blankPageIfOdd(true);
        victim.acroFormsPolicy(AcroFormPolicy.DISCARD);
        victim.outlinePolicy(OutlinePolicy.ONE_ENTRY_EACH_DOC);
        victim.tocPolicy(ToCPolicy.DOC_TITLES);
        victim.footer(true);
        victim.normalize(true);
        var source = PdfFileSource.newInstanceNoPassword(Files.createTempFile(folder, null, ".pdf").toFile());
        PdfMergeInput input = new PdfMergeInput(source);
        victim.addInput(input);
        victim.version(PdfVersion.VERSION_1_7);
        MergeParameters params = victim.build();
        assertTrue(params.isCompress());
        assertEquals(ExistingOutputPolicy.OVERWRITE, params.getExistingOutputPolicy());
        assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
        assertTrue(params.isBlankPageIfOdd());
        assertTrue(params.isFilenameFooter());
        assertTrue(params.isNormalizePageSizes());
        assertEquals(AcroFormPolicy.DISCARD, params.getAcroFormPolicy());
        assertEquals(OutlinePolicy.ONE_ENTRY_EACH_DOC, params.getOutlinePolicy());
        assertEquals(ToCPolicy.DOC_TITLES, params.getTableOfContentsPolicy());
        assertEquals(output, params.getOutput());
        assertEquals(input, params.getInputList().get(0));
    }
}
