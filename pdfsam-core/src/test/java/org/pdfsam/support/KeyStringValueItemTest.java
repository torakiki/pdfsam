/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/apr/2012
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
package org.pdfsam.support;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.pdfsam.TestUtils;

/**
 * @author Andrea Vacondio
 * 
 */
public class KeyStringValueItemTest {

    @Test(expected = IllegalArgumentException.class)
    public void invalid() {
        KeyStringValueItem.keyValue(null, "Chuck");
    }

    @Test(expected = IllegalArgumentException.class)
    public void invalidEmpty() {
        KeyStringValueItem.keyEmptyValue(null);
    }

    @Test
    public void testEquals() {
        KeyStringValueItem<String> eq1 = new KeyStringValueItem<>("A", "Chuck");
        KeyStringValueItem<String> eq2 = new KeyStringValueItem<>("A", "Norris");
        KeyStringValueItem<String> eq3 = new KeyStringValueItem<>("A", "Roundhouse");
        KeyStringValueItem<String> diff = new KeyStringValueItem<>("B", "Rambo");
        TestUtils.testEqualsAndHashCodes(eq1, eq2, eq3, diff);
    }

    @Test
    public void empty() {
        assertEquals("", KeyStringValueItem.keyEmptyValue("key").getValue());
    }
}
