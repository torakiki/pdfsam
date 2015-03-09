/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mar/2015
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
package org.pdfsam.pdfbox.component;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;

import org.apache.pdfbox.pdmodel.PDDestinationNameTreeNode;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.interactive.action.PDActionGoTo;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination.PDNamedDestination;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination.PDPageFitDestination;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDOutlineItem;
import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class PDFBoxOutlineUtilsTest {

    @Test
    public void outlineMaxDepth() throws IOException {
        try (PDDocument doc = PDDocument.load(getClass().getResourceAsStream("/pdf/test_outline.pdf"))) {
            assertEquals(3, PDFBoxOutlineUtils.getMaxBookmarkLevel(doc));
        }
    }

    @Test
    public void noOutlineMaxDepth() throws IOException {
        try (PDDocument doc = PDDocument.load(getClass().getResourceAsStream("/pdf/test_no_outline.pdf"))) {
            assertEquals(0, PDFBoxOutlineUtils.getMaxBookmarkLevel(doc));
        }
    }

    @Test
    public void toPageDestinationEmpty() {
        PDOutlineItem victim = new PDOutlineItem();
        assertFalse(PDFBoxOutlineUtils.toPageDestination(victim, null).isPresent());
    }

    @Test
    public void toPageDestinationAction() {
        PDPageFitDestination destination = new PDPageFitDestination();
        destination.setPageNumber(5);
        PDActionGoTo action = new PDActionGoTo();
        action.setDestination(destination);
        PDOutlineItem victim = new PDOutlineItem();
        victim.setAction(action);
        assertEquals(5, PDFBoxOutlineUtils.toPageDestination(victim, null).get().getPageNumber());
    }

    @Test
    public void toPageDestinationDestination() {
        PDPageFitDestination destination = new PDPageFitDestination();
        destination.setPageNumber(5);
        PDOutlineItem victim = new PDOutlineItem();
        victim.setDestination(destination);
        assertEquals(5, PDFBoxOutlineUtils.toPageDestination(victim, new PDDestinationNameTreeNode()).get()
                .getPageNumber());
    }

    @Test
    public void toPageDestinationNamedDestinationNullNames() {
        PDNamedDestination destination = new PDNamedDestination();
        PDOutlineItem victim = new PDOutlineItem();
        victim.setDestination(destination);
        assertFalse(PDFBoxOutlineUtils.toPageDestination(victim, null).isPresent());
    }

    @Test
    public void toPageDestinationNamedDestination() throws IOException {
        PDPageFitDestination dest = new PDPageFitDestination();
        dest.setPageNumber(5);
        PDNamedDestination destination = new PDNamedDestination();
        destination.setNamedDestination("ChuckNorris");
        PDOutlineItem victim = new PDOutlineItem();
        victim.setDestination(destination);
        PDDestinationNameTreeNode names = mock(PDDestinationNameTreeNode.class);
        when(names.getValue("ChuckNorris")).thenReturn(dest);
        assertEquals(5, PDFBoxOutlineUtils.toPageDestination(victim, names).get().getPageNumber());
    }

    @Test
    public void copyDictionary() {
        PDOutlineItem from = new PDOutlineItem();
        from.setBold(true);
        from.setItalic(true);
        from.setTitle("Chuck");
        PDOutlineItem to = new PDOutlineItem();
        to.setBold(false);
        to.setItalic(false);
        PDFBoxOutlineUtils.copyOutlineDictionary(from, to);
        assertTrue(to.isBold());
        assertTrue(to.isItalic());
        assertEquals("Chuck", to.getTitle());
    }
}
