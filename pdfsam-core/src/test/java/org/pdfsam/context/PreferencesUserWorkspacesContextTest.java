/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.context;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author Andrea Vacondio
 * 
 */
public class PreferencesUserWorkspacesContextTest {

    private PreferencesUserWorkspacesContext victim = new PreferencesUserWorkspacesContext();

    @Before
    public void setUp() throws InterruptedException {
        // add more workspace then max_capacity
        for (int i = 0; i < PreferencesUserWorkspacesContext.MAX_CAPACITY + 1; i++) {
            victim.addWorkspace(Integer.toString(i));
            Thread.sleep(100);
        }
    }

    @Test
    public void testSize() {
        assertEquals(PreferencesUserWorkspacesContext.MAX_CAPACITY, victim.getWorkspaces().size());
    }

    @Test
    public void testEldestIsRemoved() {
        assertFalse(victim.getWorkspaces().contains(Integer.toString(0)));
        assertTrue(victim.getWorkspaces().contains(Integer.toString(PreferencesUserWorkspacesContext.MAX_CAPACITY)));
    }

    @Test
    public void testCacheIsPopulated() {
        PreferencesUserWorkspacesContext newVictim = new PreferencesUserWorkspacesContext();
        assertEquals(PreferencesUserWorkspacesContext.MAX_CAPACITY, newVictim.getWorkspaces().size());
    }

    @Test
    public void testOrder() {
        assertEquals(PreferencesUserWorkspacesContext.MAX_CAPACITY, victim.getWorkspaces().size());
        // the first element is the most recent put
        assertEquals(0, victim.getWorkspaces().indexOf(Integer.toString(PreferencesUserWorkspacesContext.MAX_CAPACITY)));
    }
}
