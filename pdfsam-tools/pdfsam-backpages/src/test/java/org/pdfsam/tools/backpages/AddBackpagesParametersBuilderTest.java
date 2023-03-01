package org.pdfsam.tools.backpages;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.core.support.params.ConversionUtils;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.pdf.PdfVersion;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22/11/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@pdfsam.org).
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
class AddBackpagesParametersBuilderTest {

    @Test
    public void build(@TempDir Path folder) throws IOException {
        var victim = new AddBackpagesParametersBuilder();
        victim.compress(true);
        var output = mock(FileOrDirectoryTaskOutput.class);
        var backpages = PdfFileSource.newInstanceNoPassword(Files.createTempFile(folder, null, ".pdf").toFile());
        var source = PdfFileSource.newInstanceNoPassword(Files.createTempFile(folder, null, ".pdf").toFile());
        victim.output(output);
        victim.existingOutput(ExistingOutputPolicy.OVERWRITE);
        victim.discardBookmarks(true);
        victim.source(source);
        victim.backPagesSource(backpages);
        victim.version(PdfVersion.VERSION_1_7);
        victim.step(3);
        var ranges = ConversionUtils.toPageRangeSet("2,5-20,33");
        victim.ranges(ranges);
        var params = victim.build();
        assertTrue(params.isCompress());
        assertEquals(ExistingOutputPolicy.OVERWRITE, params.getExistingOutputPolicy());
        assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
        assertEquals(3, params.getStep());
        assertEquals(ranges, params.getPageSelection());
        assertEquals(output, params.getOutput());
        assertEquals(1, params.getSourceList().size());
        assertEquals(source, params.getSourceList().get(0));
        assertEquals(backpages, params.getBackPagesSource());
    }
}