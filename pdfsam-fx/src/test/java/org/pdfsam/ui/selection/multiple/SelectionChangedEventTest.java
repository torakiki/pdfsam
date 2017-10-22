/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/ago/2014
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
package org.pdfsam.ui.selection.multiple;

import static java.util.Arrays.asList;
import static java.util.Arrays.stream;
import static java.util.Collections.emptyList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.pdfsam.ui.selection.multiple.SelectionChangedEvent.clearSelectionEvent;
import static org.pdfsam.ui.selection.multiple.SelectionChangedEvent.select;

import org.junit.Test;
import org.pdfsam.ui.selection.multiple.move.MoveType;

/**
 * @author Andrea Vacondio
 *
 */
public class SelectionChangedEventTest {

    @Test(expected = IllegalArgumentException.class)
    public void nullArg() {
        select(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void negativeTotal() {
        select(emptyList()).ofTotalRows(-1);
    }

    @Test
    public void clear() {
        SelectionChangedEvent victim = clearSelectionEvent();
        assertTrue(victim.isClearSelection());
        assertFalse(victim.isSingleSelection());
    }

    @Test
    public void single() {
        SelectionChangedEvent victim = select(asList(2)).ofTotalRows(5);
        assertTrue(victim.isSingleSelection());
        assertFalse(victim.isClearSelection());
        assertEquals(2, victim.getSingleSelection());
    }

    @Test(expected = IllegalStateException.class)
    public void getSingle() {
        select(asList(2, 3)).ofTotalRows(5).getSingleSelection();
    }

    @Test
    public void clearCantMove() {
        SelectionChangedEvent victim = clearSelectionEvent();
        stream(MoveType.values()).forEach(m -> assertFalse(victim.canMove(m)));
    }

    @Test
    public void canMoveUp() {
        assertTrue(select(asList(2, 3)).ofTotalRows(5).canMove(MoveType.UP));
    }

    @Test
    public void cantMoveUp() {
        assertFalse(select(asList(0, 3)).ofTotalRows(5).canMove(MoveType.UP));
    }

    @Test
    public void cantMoveBottom() {
        assertFalse(select(asList(0, 3)).ofTotalRows(5).canMove(MoveType.BOTTOM));
        assertFalse(select(asList(4)).ofTotalRows(5).canMove(MoveType.BOTTOM));
    }

    @Test
    public void canMoveBottom() {
        assertTrue(select(asList(2)).ofTotalRows(5).canMove(MoveType.BOTTOM));
    }

    @Test
    public void cantMoveDown() {
        assertFalse(select(asList(2, 4)).ofTotalRows(5).canMove(MoveType.DOWN));
    }

    @Test
    public void canMoveDown() {
        assertTrue(select(asList(0, 2)).ofTotalRows(5).canMove(MoveType.DOWN));
    }

    @Test
    public void cantMoveTop() {
        assertFalse(select(asList(0, 3)).ofTotalRows(5).canMove(MoveType.TOP));
        assertFalse(select(asList(0)).ofTotalRows(5).canMove(MoveType.TOP));
    }

    @Test
    public void canMoveTop() {
        assertTrue(select(asList(3)).ofTotalRows(5).canMove(MoveType.TOP));
    }
}
