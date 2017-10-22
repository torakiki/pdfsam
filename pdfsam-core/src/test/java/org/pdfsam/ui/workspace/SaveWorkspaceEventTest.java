/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/dic/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.workspace;

import static java.util.Objects.isNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.File;

import org.junit.Before;
import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
@SuppressWarnings("unused")
public class SaveWorkspaceEventTest {

    private SaveWorkspaceEvent victim;

    @Before
    public void setUp() {
        victim = new SaveWorkspaceEvent(mock(File.class));
    }

    @Test(expected = IllegalArgumentException.class)
    public void requiredFile() {
        new SaveWorkspaceEvent(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void nonNullKey() {
        victim.addValue("module", null, "value");
    }

    @Test
    public void nullValue() {
        victim.addValue("module", "key", null);
        assertEquals("", victim.getDataForModule("module").get("key"));
    }

    @Test
    public void add() {
        assertTrue(victim.getData().isEmpty());
        victim.addValue("module", "key", "value");
        assertFalse(victim.getData().isEmpty());
    }

    @Test
    public void addToTheRightModule() {
        victim.addValue("module", "key", "value");
        assertFalse(victim.getData().get("module").isEmpty());
        assertTrue(isNull(victim.getData().get("chuck")));
    }

    @Test
    public void multipleAdd() {
        victim.addValue("module", "key", "value");
        victim.addValue("module", "key2", "value2");
        assertEquals(2, victim.getDataForModule("module").size());
    }

    @Test
    public void nullSafeGet() {
        assertEquals(0, victim.getDataForModule("chuck").size());
    }
}
