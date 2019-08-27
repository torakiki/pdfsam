/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 27/ago/2019
 * Copyright 2019 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import java.io.File;

import org.junit.Test;

public class NumericalSortFilenameComparatorTest {

    @Test
    public void nulls() {
        assertEquals(0, new NumericalSortFilenameComparator().compare(null, new File("bla")));
        assertEquals(0, new NumericalSortFilenameComparator().compare(null, null));
        assertEquals(0, new NumericalSortFilenameComparator().compare(new File("bla"), null));
    }

    @Test
    public void nonDigit() {
        assertEquals(0, new NumericalSortFilenameComparator().compare(new File("123.pdf"), new File("bla.pdf")));
        assertEquals(0, new NumericalSortFilenameComparator().compare(new File("bla"), new File("123.pdf")));
    }

    @Test
    public void digits() {
        assertEquals(1, new NumericalSortFilenameComparator().compare(new File("123.pdf"), new File("1bla.pdf")));
        assertEquals(1, new NumericalSortFilenameComparator().compare(new File("123bla.pdf"), new File("1bla.pdf")));
        assertEquals(0, new NumericalSortFilenameComparator().compare(new File("1.pdf"), new File("001bla.pdf")));
        assertEquals(0, new NumericalSortFilenameComparator().compare(new File("1bla.pdf"), new File("001bla.pdf")));
        assertEquals(-1, new NumericalSortFilenameComparator().compare( new File("005bla.pdf"), new File("500.pdf")));
        assertEquals(-1, new NumericalSortFilenameComparator().compare(new File("005bla.pdf"), new File("500bla.pdf")));
    }
}
