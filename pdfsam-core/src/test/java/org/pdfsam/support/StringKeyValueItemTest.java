/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/apr/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.support;

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author Andrea Vacondio
 * 
 */
public class StringKeyValueItemTest {

    @Test(expected = IllegalArgumentException.class)
    public void testInvalidConstructor() {
        new StringKeyValueItem("", "Chuck");
    }

    @Test
    public void testEquals() {
        StringKeyValueItem item = new StringKeyValueItem("A", "Chuck");
        StringKeyValueItem item2 = new StringKeyValueItem("A", "Norris");
        StringKeyValueItem item3 = new StringKeyValueItem("B", "Rambo");
        assertTrue(item.equals(item2));
        assertFalse(item.equals(item3));
    }
}
