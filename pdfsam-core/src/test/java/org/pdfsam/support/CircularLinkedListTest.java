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
package org.pdfsam.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class CircularLinkedListTest {

    @Test(expected = IllegalArgumentException.class)
    public void wrongCapacity() {
        new CircularLinkedList<>(0);
    }

    @Test
    public void add() {
        CircularLinkedList<Integer> victim = new CircularLinkedList<>(2);
        assertTrue(victim.add(1));
        assertTrue(victim.add(2));
        assertTrue(victim.add(3));
        assertEquals(2, victim.size());
        assertTrue(victim.isFull());
        assertFalse(victim.contains(1));
    }

    @Test
    public void offer() {
        CircularLinkedList<Integer> victim = new CircularLinkedList<>(2);
        assertTrue(victim.offer(1));
        assertTrue(victim.offer(2));
        assertTrue(victim.offer(3));
        assertEquals(2, victim.size());
        assertTrue(victim.isFull());
        assertFalse(victim.contains(1));
    }

    @Test
    public void addLast() {
        CircularLinkedList<Integer> victim = new CircularLinkedList<>(2);
        victim.addLast(1);
        victim.addLast(2);
        victim.addLast(3);
        assertEquals(2, victim.size());
        assertTrue(victim.isFull());
        assertFalse(victim.contains(1));
    }

    @Test
    public void offerLast() {
        CircularLinkedList<Integer> victim = new CircularLinkedList<>(2);
        victim.offerLast(1);
        victim.offerLast(2);
        victim.offerLast(3);
        assertEquals(2, victim.size());
        assertTrue(victim.isFull());
        assertFalse(victim.contains(1));
    }

    @Test
    public void addFirst() {
        CircularLinkedList<Integer> victim = new CircularLinkedList<>(2);
        victim.addFirst(1);
        victim.addFirst(2);
        victim.addFirst(3);
        assertEquals(2, victim.size());
        assertTrue(victim.isFull());
        assertFalse(victim.contains(2));
    }

    @Test
    public void offerFirst() {
        CircularLinkedList<Integer> victim = new CircularLinkedList<>(2);
        victim.offerFirst(1);
        victim.offerFirst(2);
        victim.offerFirst(3);
        assertEquals(2, victim.size());
        assertTrue(victim.isFull());
        assertFalse(victim.contains(2));
    }

    @Test
    public void push() {
        CircularLinkedList<Integer> victim = new CircularLinkedList<>(2);
        victim.push(1);
        victim.push(2);
        victim.push(3);
        assertEquals(2, victim.size());
        assertTrue(victim.isFull());
        assertFalse(victim.contains(2));
    }

    @Test
    public void addAll() {
        CircularLinkedList<Integer> victim = new CircularLinkedList<>(2);
        victim.addAll(Arrays.asList(1, 2, 3));
        assertEquals(2, victim.size());
        assertTrue(victim.isFull());
        assertFalse(victim.contains(1));
    }

    @Test
    public void addAllIndex() {
        CircularLinkedList<Integer> victim = new CircularLinkedList<>(3);
        victim.add(100);
        victim.add(101);
        victim.add(102);
        victim.addAll(1, Arrays.asList(1, 2, 3));
        assertEquals(3, victim.size());
        assertTrue(victim.isFull());
        assertFalse(victim.contains(100));
        assertFalse(victim.contains(1));
        assertFalse(victim.contains(2));
        assertEquals(Integer.valueOf(3), victim.get(0));
        assertEquals(Integer.valueOf(101), victim.get(1));
        assertEquals(Integer.valueOf(102), victim.get(2));
    }

    @Test
    public void addIndex() {
        CircularLinkedList<Integer> victim = new CircularLinkedList<>(3);
        victim.add(1);
        victim.add(2);
        victim.add(3);
        victim.add(1, 100);
        assertEquals(3, victim.size());
        assertTrue(victim.isFull());
        assertFalse(victim.contains(1));
        assertEquals(Integer.valueOf(100), victim.get(0));
        assertEquals(Integer.valueOf(2), victim.get(1));
        assertEquals(Integer.valueOf(3), victim.get(2));
    }
}
