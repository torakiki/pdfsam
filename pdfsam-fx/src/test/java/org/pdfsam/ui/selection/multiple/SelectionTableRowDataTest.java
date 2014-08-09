/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/ago/2014
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
package org.pdfsam.ui.selection.multiple;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Set;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.pdf.page.PageRange;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(MockitoJUnitRunner.class)
public class SelectionTableRowDataTest {

    @Rule
    public ClearEventStudioRule eventStudioRule = new ClearEventStudioRule();
    @Mock
    private File file;

    @Test
    public void empty() {
        assertTrue(new SelectionTableRowData(file).toPageRangeSet().isEmpty());
    }

    @Test(expected = ConversionException.class)
    public void invalid() {
        SelectionTableRowData victim = new SelectionTableRowData(file);
        victim.setPageSelection("Chuck Norris");
        victim.toPageRangeSet();
    }

    @Test(expected = ConversionException.class)
    public void invalidRange() {
        SelectionTableRowData victim = new SelectionTableRowData(file);
        victim.setPageSelection("1-2-3");
        victim.toPageRangeSet();
    }

    @Test(expected = ConversionException.class)
    public void endLower() {
        SelectionTableRowData victim = new SelectionTableRowData(file);
        victim.setPageSelection("10-5");
        victim.toPageRangeSet();
    }

    @Test
    public void singlePage() {
        SelectionTableRowData victim = new SelectionTableRowData(file);
        victim.setPageSelection("5");
        Set<PageRange> pageSet = victim.toPageRangeSet();
        assertEquals(1, pageSet.size());
        assertEquals(5, pageSet.stream().findFirst().get().getStart());
        assertEquals(5, pageSet.stream().findFirst().get().getEnd());
    }

    @Test
    public void rangePage() {
        SelectionTableRowData victim = new SelectionTableRowData(file);
        victim.setPageSelection("5-10");
        Set<PageRange> pageSet = victim.toPageRangeSet();
        assertEquals(1, pageSet.size());
        assertEquals(5, pageSet.stream().findFirst().get().getStart());
        assertEquals(10, pageSet.stream().findFirst().get().getEnd());
    }

    @Test
    public void endPage() {
        SelectionTableRowData victim = new SelectionTableRowData(file);
        victim.setPageSelection("-10");
        Set<PageRange> pageSet = victim.toPageRangeSet();
        assertEquals(1, pageSet.size());
        assertEquals(1, pageSet.stream().findFirst().get().getStart());
        assertEquals(10, pageSet.stream().findFirst().get().getEnd());
    }

    @Test
    public void startPage() {
        SelectionTableRowData victim = new SelectionTableRowData(file);
        victim.setPageSelection("10-");
        Set<PageRange> pageSet = victim.toPageRangeSet();
        assertEquals(1, pageSet.size());
        assertEquals(10, pageSet.stream().findFirst().get().getStart());
        assertTrue(pageSet.stream().findFirst().get().isUnbounded());
    }

    @Test
    public void multiple() {
        SelectionTableRowData victim = new SelectionTableRowData(file);
        victim.setPageSelection("2-4,10-");
        Set<PageRange> pageSet = victim.toPageRangeSet();
        assertEquals(2, pageSet.size());
        assertEquals(2, pageSet.stream().findFirst().get().getStart());
        assertEquals(4, pageSet.stream().findFirst().get().getEnd());
        assertTrue(pageSet.stream().anyMatch(PageRange::isUnbounded));
    }
}
