/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/giu/2013
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
package org.pdfsam.gui.view.selection;

import org.junit.Test;
import org.pdfsam.gui.event.String;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import static org.pdfsam.gui.view.selection.SelectionChangedEvent.selectionChanged;

/**
 * @author Andrea Vacondio
 * 
 */
public class SelectionChangedEventTest {

    private String namespace = String.newParentInstance("/");

    @Test
    public void clear() {
        assertTrue(selectionChanged(namespace).clearSelection().isClearSelection());
    }

    @Test
    public void single() {
        assertTrue(selectionChanged(namespace).select(3).isSingleSelection());
    }

    @Test
    public void range() {
        SelectionChangedEvent victim = selectionChanged(namespace).startSelectionAt(1).endSelectionAt(10)
                .ofTotalRows(20);
        assertEquals(1, victim.getStartIndex());
        assertEquals(10, victim.getEndIndex());
        assertEquals(20, victim.getTotalRows());
        assertFalse(victim.isClearSelection());
        assertFalse(victim.isSingleSelection());
    }

    @Test(expected = IllegalArgumentException.class)
    public void negativeSingle() {
        selectionChanged(namespace).select(-1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void negativeStart() {
        selectionChanged(namespace).startSelectionAt(-1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void negativeEnd() {
        selectionChanged(namespace).startSelectionAt(1).endSelectionAt(-10);
    }

    @Test(expected = IllegalArgumentException.class)
    public void mismatchedStartEnd() {
        selectionChanged(namespace).startSelectionAt(10).endSelectionAt(1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void negativeTotal() {
        selectionChanged(namespace).startSelectionAt(1).endSelectionAt(10).ofTotalRows(-10);
    }
}
