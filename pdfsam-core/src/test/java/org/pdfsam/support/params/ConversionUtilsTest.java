/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22 giu 2016
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
package org.pdfsam.support.params;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Set;

import org.junit.Test;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.pdf.page.PageRange;

/**
 * @author Andrea Vacondio
 *
 */
public class ConversionUtilsTest {

    @Test(expected = ConversionException.class)
    public void invalid() {
        ConversionUtils.toPageRangeSet("Chuck Norris");
    }

    @Test(expected = ConversionException.class)
    public void invalidRange() {
        ConversionUtils.toPageRangeSet("1-2-3");
    }

    @Test(expected = ConversionException.class)
    public void endLower() {
        ConversionUtils.toPageRangeSet("10-5");
    }

    @Test
    public void singlePage() {
        Set<PageRange> pageSet = ConversionUtils.toPageRangeSet("5");
        assertEquals(1, pageSet.size());
        assertEquals(5, pageSet.stream().findFirst().get().getStart());
        assertEquals(5, pageSet.stream().findFirst().get().getEnd());
    }

    @Test
    public void rangePage() {
        Set<PageRange> pageSet = ConversionUtils.toPageRangeSet("5-10");
        assertEquals(1, pageSet.size());
        assertEquals(5, pageSet.stream().findFirst().get().getStart());
        assertEquals(10, pageSet.stream().findFirst().get().getEnd());
    }

    @Test
    public void endPage() {
        Set<PageRange> pageSet = ConversionUtils.toPageRangeSet("-10");
        assertEquals(1, pageSet.size());
        assertEquals(1, pageSet.stream().findFirst().get().getStart());
        assertEquals(10, pageSet.stream().findFirst().get().getEnd());
    }

    @Test
    public void startPage() {
        Set<PageRange> pageSet = ConversionUtils.toPageRangeSet("10-");
        assertEquals(1, pageSet.size());
        assertEquals(10, pageSet.stream().findFirst().get().getStart());
        assertTrue(pageSet.stream().findFirst().get().isUnbounded());
    }

    @Test
    public void multiple() {
        Set<PageRange> pageSet = ConversionUtils.toPageRangeSet("2-4,10-");
        assertEquals(2, pageSet.size());
        assertEquals(2, pageSet.stream().findFirst().get().getStart());
        assertEquals(4, pageSet.stream().findFirst().get().getEnd());
        assertTrue(pageSet.stream().anyMatch(PageRange::isUnbounded));
    }
}
