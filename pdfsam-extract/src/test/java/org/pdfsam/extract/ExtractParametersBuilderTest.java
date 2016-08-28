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
package org.pdfsam.extract;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.IOException;
import java.util.Set;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.support.params.ConversionUtils;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.optimization.OptimizationPolicy;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.parameter.ExtractPagesParameters;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.page.PageRange;

/**
 * @author Andrea Vacondio
 *
 */
public class ExtractParametersBuilderTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void build() throws IOException {
        ExtractParametersBuilder victim = new ExtractParametersBuilder();
        victim.compress(true);
        DirectoryTaskOutput output = mock(DirectoryTaskOutput.class);
        victim.output(output);
        victim.existingOutput(ExistingOutputPolicy.OVERWRITE);
        victim.discardBookmarks(true);
        victim.prefix("prefix");
        victim.invertSelection(true);
        PdfFileSource source = PdfFileSource.newInstanceNoPassword(folder.newFile("my.pdf"));
        victim.addSource(source);
        victim.addSource(PdfFileSource.newInstanceNoPassword(folder.newFile("mytest.pdf")));
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
        assertEquals("prefix", params.getOutputPrefix());
        assertEquals(2, params.getSourceList().size());
        assertEquals(source, params.getSourceList().get(0));
    }
}
