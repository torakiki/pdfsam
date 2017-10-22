/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23/gen/2015
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
package org.pdfsam.ui.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;
import org.pdfsam.support.CircularLinkedList;

/**
 * @author Andrea Vacondio
 *
 */
public class CircularObservableListTest {
    @Test(expected = IllegalArgumentException.class)
    public void wrongCapacity() {
        new CircularLinkedList<>(0);
    }

    @Test
    public void add() {
        CircularObservableList<Integer> victim = new CircularObservableList<>(2);
        assertTrue(victim.add(1));
        assertTrue(victim.add(2));
        assertTrue(victim.add(3));
        assertEquals(2, victim.size());
        assertFalse(victim.contains(1));
    }

    @Test
    public void addAll() {
        CircularObservableList<Integer> victim = new CircularObservableList<>(2);
        victim.addAll(Arrays.asList(1, 2, 3));
        assertEquals(2, victim.size());
        assertFalse(victim.contains(1));
    }

    @Test
    public void addAllIndex() {
        CircularObservableList<Integer> victim = new CircularObservableList<>(3);
        victim.add(100);
        victim.add(101);
        victim.add(102);
        victim.add(103);
        victim.addAll(1, Arrays.asList(1, 2, 3));
        assertEquals(3, victim.size());
        assertFalse(victim.contains(100));
        assertFalse(victim.contains(1));
        assertFalse(victim.contains(2));
        assertEquals(Integer.valueOf(3), victim.get(0));
        assertEquals(Integer.valueOf(102), victim.get(1));
        assertEquals(Integer.valueOf(103), victim.get(2));
    }

    @Test
    public void addIndex() {
        CircularObservableList<Integer> victim = new CircularObservableList<>(3);
        victim.add(1);
        victim.add(2);
        victim.add(3);
        victim.add(1, 100);
        assertEquals(3, victim.size());
        assertFalse(victim.contains(1));
        assertEquals(Integer.valueOf(100), victim.get(0));
        assertEquals(Integer.valueOf(2), victim.get(1));
        assertEquals(Integer.valueOf(3), victim.get(2));
    }
}
