/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
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
package org.pdfsam.tools.extract;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.core.support.params.ConversionUtils;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.optimization.OptimizationPolicy;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.parameter.ExtractPagesParameters;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.page.PageRange;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 *
 */
public class ExtractParametersBuilderTest {

    @Test
    public void build(@TempDir Path folder) throws IOException {
        var victim = new ExtractParametersBuilder();
        victim.compress(true);
        FileOrDirectoryTaskOutput output = mock(FileOrDirectoryTaskOutput.class);
        victim.output(output);
        victim.existingOutput(ExistingOutputPolicy.OVERWRITE);
        victim.discardBookmarks(true);
        victim.prefix("prefix");
        victim.invertSelection(true);
        victim.separateForEachRange(true);
        PdfFileSource source = PdfFileSource.newInstanceNoPassword(Files.createTempFile(folder, null, ".pdf").toFile());
        victim.addSource(source);
        victim.addSource(PdfFileSource.newInstanceNoPassword(Files.createTempFile(folder, null, ".pdf").toFile()));
        victim.version(PdfVersion.VERSION_1_7);
        Set<PageRange> ranges = ConversionUtils.toPageRangeSet("2,5-20,33");
        victim.ranges(ranges);
        ExtractPagesParameters params = victim.build();
        assertTrue(params.isCompress());
        assertTrue(params.discardOutline());
        assertEquals(ExistingOutputPolicy.OVERWRITE, params.getExistingOutputPolicy());
        assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
        assertEquals(OptimizationPolicy.AUTO, params.getOptimizationPolicy());
        assertEquals(ranges, params.getPageSelection());
        assertEquals(output, params.getOutput());
        assertTrue(params.isInvertSelection());
        assertTrue(params.isSeparateFileForEachRange());
        assertEquals("prefix", params.getOutputPrefix());
        assertEquals(2, params.getSourceList().size());
        assertEquals(source, params.getSourceList().get(0));
    }
}
