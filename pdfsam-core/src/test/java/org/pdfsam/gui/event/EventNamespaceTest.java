/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/feb/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.gui.event;

import org.junit.Test;
import org.pdfsam.TestUtils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author Andrea Vacondio
 * 
 */
public class EventNamespaceTest {

    @Test(expected = IllegalArgumentException.class)
    public void testBlank() {
        EventNamespace.newParentInstance("");
    }

    @Test
    public void testParent() {
        EventNamespace victim = EventNamespace.newParentInstance("parent");
        assertEquals("parent", victim.getNamespaceId());
    }

    @Test
    public void testChild() {
        EventNamespace root = EventNamespace.newParentInstance("root");
        EventNamespace victim = EventNamespace.newChildInstance(root, "child");
        assertEquals("root.child", victim.getNamespaceId());
    }

    @Test
    public void testIsParentForNull() {
        assertTrue(EventNamespace.NULL.isParentOf(EventNamespace.newParentInstance("root")));
    }

    @Test
    public void testIsParentOf() {
        EventNamespace root = EventNamespace.newParentInstance("root");
        EventNamespace child = EventNamespace.newChildInstance(root, "child");
        assertTrue(root.isParentOf(child));
    }

    @Test
    public void testIsParentOfItself() {
        EventNamespace root = EventNamespace.newParentInstance("root");
        assertTrue(root.isParentOf(root));
    }

    @Test
    public void testEquals() {
        EventNamespace eq1 = EventNamespace.newParentInstance("root");
        EventNamespace eq2 = EventNamespace.newParentInstance("root");
        EventNamespace eq3 = EventNamespace.newParentInstance("root");
        EventNamespace diff = EventNamespace.newChildInstance(eq1, "child");
        TestUtils.testEqualsAndHashCodes(eq1, eq2, eq3, diff);
    }
}
