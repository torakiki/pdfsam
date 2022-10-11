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
package org.pdfsam.ui.components.selection.multiple;

import org.junit.jupiter.api.Test;
import org.pdfsam.ui.components.selection.multiple.move.MoveType;

import java.util.Arrays;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.ui.components.selection.multiple.SelectionChangedEvent.clearSelectionEvent;
import static org.pdfsam.ui.components.selection.multiple.SelectionChangedEvent.select;

/**
 * @author Andrea Vacondio
 */
public class SelectionChangedEventTest {

    @Test
    public void nullArg() {
        assertThrows(IllegalArgumentException.class, () -> select(null));

    }

    @Test
    public void negativeTotal() {
        assertThrows(IllegalArgumentException.class, () -> select(emptyList()).ofTotalRows(-1));
    }

    @Test
    public void clear() {
        SelectionChangedEvent victim = clearSelectionEvent();
        assertTrue(victim.isClearSelection());
        assertFalse(victim.isSingleSelection());
    }

    @Test
    public void single() {
        SelectionChangedEvent victim = select(List.of(2)).ofTotalRows(5);
        assertTrue(victim.isSingleSelection());
        assertFalse(victim.isClearSelection());
        assertEquals(2, victim.getSingleSelection());
    }

    @Test
    public void getSingle() {
        assertThrows(IllegalStateException.class, () -> select(asList(2, 3)).ofTotalRows(5).getSingleSelection());
    }

    @Test
    public void clearCantMove() {
        SelectionChangedEvent victim = clearSelectionEvent();
        Arrays.stream(MoveType.values()).forEach(m -> assertFalse(victim.canMove(m)));
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
        assertFalse(select(asList(2, 4)).ofTotalRows(5).canMove(MoveType.BOTTOM));
        assertFalse(select(List.of(4)).ofTotalRows(5).canMove(MoveType.BOTTOM));
    }

    @Test
    public void canMoveBottom() {
        assertTrue(select(asList(0, 3)).ofTotalRows(5).canMove(MoveType.BOTTOM));
        assertTrue(select(List.of(2)).ofTotalRows(5).canMove(MoveType.BOTTOM));
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
        assertFalse(select(List.of(0)).ofTotalRows(5).canMove(MoveType.TOP));
    }

    @Test
    public void canMoveTop() {
        assertTrue(select(asList(1, 3)).ofTotalRows(5).canMove(MoveType.TOP));
        assertTrue(select(List.of(3)).ofTotalRows(5).canMove(MoveType.TOP));
    }
}
