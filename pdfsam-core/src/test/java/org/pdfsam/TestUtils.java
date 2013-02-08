/*
 * Created on 08/feb/2013
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author Andrea Vacondio
 * 
 */
public class TestUtils {
    private static final NotInstanceOf NOT_INSTANCE_OF = new NotInstanceOf();

    /**
     * Test that the equals and hashCode implementations respect the general rules being reflexive, transitive and symmetric.
     * 
     * @param <T>
     * @param eq1
     *            equal instance
     * @param eq2
     *            equal instance
     * @param eq3
     *            equal instance
     * @param diff
     *            not equal instance
     */
    public static <T> void testEqualsAndHashCodes(T eq1, T eq2, T eq3, T diff) {
        // not instance of
        assertFalse(eq1.equals(NOT_INSTANCE_OF));

        // reflexive
        assertTrue(eq1.equals(eq1));
        assertTrue(eq1.hashCode() == eq1.hashCode());

        // symmetric
        assertTrue(eq1.equals(eq2));
        assertTrue(eq2.equals(eq1));
        assertTrue(eq1.hashCode() == eq2.hashCode());
        assertFalse(eq2.equals(diff));
        assertFalse(diff.equals(eq2));
        assertFalse(diff.hashCode() == eq2.hashCode());

        // transitive
        assertTrue(eq1.equals(eq2));
        assertTrue(eq2.equals(eq3));
        assertTrue(eq1.equals(eq3));
        assertTrue(eq1.hashCode() == eq2.hashCode());
        assertTrue(eq2.hashCode() == eq3.hashCode());
        assertTrue(eq1.hashCode() == eq3.hashCode());
    }

    /**
     * Class used to test instance of returning false.
     * 
     * @author Andrea Vacondio
     * 
     */
    private static final class NotInstanceOf {
        // nothing
    }
}
