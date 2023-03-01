/*
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mar/2015
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.service.pdf;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.sejda.io.SeekableSources;
import org.sejda.sambox.input.PDFParser;
import org.sejda.sambox.pdmodel.PDDocument;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Andrea Vacondio
 */
public class BookmarksLevelSAMBoxLoaderTest {

    @Test
    public void accept(@TempDir Path folder) throws IOException {
        var path = Files.createTempFile(folder, null, ".pdf");
        Files.copy(getClass().getResourceAsStream("/test_outline.pdf"), path, StandardCopyOption.REPLACE_EXISTING);
        var descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(path.toFile());
        try (PDDocument document = PDFParser.parse(SeekableSources.seekableSourceFrom(path))) {
            new BookmarksLevelSAMBoxLoader().accept(document, descriptor);
            assertThat(descriptor.getValidBookmarksLevels()).containsExactly(1, 2, 3);
        }
    }
}
