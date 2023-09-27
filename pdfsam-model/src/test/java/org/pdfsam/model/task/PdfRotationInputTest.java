/*
 * Created on 21 giu 2016
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
 * This file is part of Sejda.
 *
 * Sejda is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Sejda is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Sejda.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.model.task;

import org.junit.jupiter.api.Test;
import org.sejda.model.input.PdfSource;
import org.sejda.model.pdf.page.PageRange;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfRotationInputTest {

    @Test
    public void nullSource() {
        assertThrows(NullPointerException.class, () -> new PdfRotationInput(null, Rotation.DEGREES_180));
    }

    @Test
    public void nullRotation() {
        assertThrows(NullPointerException.class, () -> new PdfRotationInput(mock(PdfSource.class), null));
    }

    @Test
    public void nullPagesAllPages() {
        PdfRotationInput victim = new PdfRotationInput(mock(PdfSource.class), Rotation.DEGREES_180, null);
        Set<Integer> pages = victim.getPages(3);
        assertEquals(3, pages.size());
        assertTrue(pages.contains(1));
        assertTrue(pages.contains(2));
        assertTrue(pages.contains(3));
    }

    @Test
    public void emptyPagesAllPages() {
        PdfRotationInput victim = new PdfRotationInput(mock(PdfSource.class), Rotation.DEGREES_180);
        Set<Integer> pages = victim.getPages(3);
        assertEquals(3, pages.size());
        assertTrue(pages.contains(1));
        assertTrue(pages.contains(2));
        assertTrue(pages.contains(3));
    }

    @Test
    public void predefinedPageSet() {
        PdfRotationInput victim = new PdfRotationInput(mock(PdfSource.class), Rotation.DEGREES_180,
                PredefinedSetOfPages.EVEN_PAGES);
        Set<Integer> pages = victim.getPages(3);
        assertEquals(1, pages.size());
        assertTrue(pages.contains(2));
    }

    @Test
    public void multiplePageRanges() {
        PdfRotationInput victim = new PdfRotationInput(mock(PdfSource.class), Rotation.DEGREES_180,
                new PageRange[] { new PageRange(2, 3), new PageRange(5, 5) });
        Set<Integer> pages = victim.getPages(5);
        assertEquals(3, pages.size());
        assertTrue(pages.contains(2));
        assertTrue(pages.contains(3));
        assertTrue(pages.contains(5));
    }
}
