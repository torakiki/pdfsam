/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/mar/2015
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
package org.pdfsam.pdfbox;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.junit.Test;
import org.pdfsam.pdfbox.PDFBoxOutlineLevelsHandler;
import org.sejda.model.outline.OutlineLevelsHandler;

/**
 * @author Andrea Vacondio
 *
 */
public class PDFBoxOutlineLevelsHandlerTest {
    @Test
    public void positiveGetGoToBookmarkMaxDepth() throws IOException {
        try (PDDocument document = PDDocument.load(getClass().getClassLoader().getResourceAsStream(
                "pdf/test_outline.pdf"))) {
            OutlineLevelsHandler victim = new PDFBoxOutlineLevelsHandler(document, null);
            assertEquals(3, victim.getMaxOutlineDepth());
        }
    }

    @Test
    public void negativeGetGoToBookmarkMaxDepth() throws IOException {
        try (PDDocument document = PDDocument.load(getClass().getClassLoader().getResourceAsStream(
                "pdf/test_no_outline.pdf"))) {
            OutlineLevelsHandler victim = new PDFBoxOutlineLevelsHandler(document, null);
            assertEquals(0, victim.getMaxOutlineDepth());
        }
    }

    @Test
    public void getPageNumbersAtOutlineLevel() throws IOException {
        try (PDDocument document = PDDocument.load(getClass().getClassLoader().getResourceAsStream(
                "pdf/test_outline.pdf"))) {
            OutlineLevelsHandler victim = new PDFBoxOutlineLevelsHandler(document, null);
            assertTrue(victim.getPageDestinationsForLevel(4).getPages().isEmpty());
            assertEquals(2, victim.getPageDestinationsForLevel(2).getPages().size());
            assertEquals(1, victim.getPageDestinationsForLevel(3).getPages().size());
        }
    }

    @Test
    public void getPageNumbersAtOutlineLevelNoOutline() throws IOException {
        try (PDDocument document = PDDocument.load(getClass().getClassLoader().getResourceAsStream(
                "pdf/test_no_outline.pdf"))) {
            OutlineLevelsHandler victim = new PDFBoxOutlineLevelsHandler(document, null);
            assertEquals(0, victim.getPageDestinationsForLevel(2).getPages().size());
        }
    }

    @Test
    public void getPageNumbersAtOutlineLevelMatching() throws IOException {
        try (PDDocument document = PDDocument.load(getClass().getClassLoader().getResourceAsStream(
                "pdf/test_outline.pdf"))) {
            OutlineLevelsHandler victim = new PDFBoxOutlineLevelsHandler(document, "(.+)page(.*)");
            assertEquals(1, victim.getPageDestinationsForLevel(2).getPages().size());
        }
    }
}
