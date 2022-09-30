/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 06/ago/2014
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
package org.pdfsam.ui.components.selection.multiple.move;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Andrea Vacondio
 *
 */
public class MoveTypeTest {

    private PdfDocumentDescriptor first;
    private PdfDocumentDescriptor second;
    private PdfDocumentDescriptor third;
    private PdfDocumentDescriptor fourth;
    private ObservableList<PdfDocumentDescriptor> items;

    @BeforeEach
    public void setUp() {
        first = Mockito.mock(PdfDocumentDescriptor.class);
        second = Mockito.mock(PdfDocumentDescriptor.class);
        third = Mockito.mock(PdfDocumentDescriptor.class);
        fourth = Mockito.mock(PdfDocumentDescriptor.class);
        items = FXCollections.observableArrayList(first, second, third, fourth);
    }

    @Test
    public void moveTopFirst() {
        Integer[] indices = new Integer[] { 0 };
        assertEquals(SelectionAndFocus.NULL, MoveType.TOP.move(indices, items, -1));
    }

    @Test
    public void moveTopContinuosMultipleSelection() {
        Integer[] indices = new Integer[] { 2, 3 };
        SelectionAndFocus selection = MoveType.TOP.move(indices, items, 2);
        assertThat(items).containsExactly(third, fourth, first, second);
        assertEquals(0, selection.getFocus());
        assertEquals(0, selection.getRow());
        assertArrayEquals(new int[] { 1 }, selection.getRows());
    }

    @Test
    public void moveTopMultipleSelection() {
        Integer[] indices = new Integer[] { 1, 3 };
        SelectionAndFocus selection = MoveType.TOP.move(indices, items, 3);
        assertThat(items).containsExactly(second, fourth, first, third);
        assertEquals(1, selection.getFocus());
        assertEquals(0, selection.getRow());
        assertArrayEquals(new int[] { 1 }, selection.getRows());
    }

    @Test
    public void moveTop() {
        Integer[] indices = new Integer[] { 1 };
        SelectionAndFocus selection = MoveType.TOP.move(indices, items, -1);
        assertEquals(second, items.get(0));
        assertEquals(-1, selection.getFocus());
        assertEquals(0, selection.getRow());
    }

    @Test
    public void moveBottomLast() {
        Integer[] indices = new Integer[] { 3 };
        assertEquals(SelectionAndFocus.NULL, MoveType.BOTTOM.move(indices, items, -1));
    }

    @Test
    public void moveBottomContinuosMultipleSelection() {
        Integer[] indices = new Integer[] { 1, 2 };
        SelectionAndFocus selection = MoveType.BOTTOM.move(indices, items, 1);
        assertThat(items).containsExactly(first, fourth, second, third);
        assertEquals(2, selection.getFocus());
        assertEquals(2, selection.getRow());
        assertArrayEquals(new int[] { 3 }, selection.getRows());
    }

    @Test
    public void moveBottomMultipleSelection() {
        Integer[] indices = new Integer[] { 0, 2 };
        SelectionAndFocus selection = MoveType.BOTTOM.move(indices, items, 2);
        assertThat(items).containsExactly(second, fourth, first, third);
        assertEquals(3, selection.getFocus());
        assertEquals(2, selection.getRow());
        assertArrayEquals(new int[] { 3 }, selection.getRows());
    }

    @Test
    public void moveBottom() {
        Integer[] indices = new Integer[] { 1 };
        MoveType.BOTTOM.move(indices, items, -1);
        assertEquals(second, items.get(3));
    }

    @Test
    public void moveFullSelection() {
        Integer[] indices = new Integer[] { 0, 1, 2, 3 };
        assertEquals(SelectionAndFocus.NULL, MoveType.UP.move(indices, items, -1));
        assertEquals(SelectionAndFocus.NULL, MoveType.DOWN.move(indices, items, -1));
    }

    @Test
    public void moveUpFirst() {
        Integer[] indices = new Integer[] { 2, 0 };
        assertEquals(SelectionAndFocus.NULL, MoveType.UP.move(indices, items, -1));
    }

    @Test
    public void moveUpContinuosMultipleSelection() {
        Integer[] indices = new Integer[] { 1, 2 };
        MoveType.UP.move(indices, items, -1);
        assertThat(items).containsExactly(second, third, first, fourth);
    }

    @Test
    public void moveUpMultipleSelection() {
        Integer[] indices = new Integer[] { 1, 3 };
        SelectionAndFocus selection = MoveType.UP.move(indices, items, 3);
        assertThat(items).containsExactly(second, first, fourth, third);
        assertEquals(2, selection.getFocus());
        assertEquals(0, selection.getRow());
        assertArrayEquals(new int[] { 2 }, selection.getRows());
    }

    @Test
    public void moveUpSingleSelection() {
        Integer[] indices = new Integer[] { 2 };
        MoveType.UP.move(indices, items, -1);
        assertThat(items).containsExactly(first, third, second, fourth);
    }

    @Test
    public void moveDownLast() {
        Integer[] indices = new Integer[] { 1, 3 };
        assertEquals(SelectionAndFocus.NULL, MoveType.DOWN.move(indices, items, -1));
    }

    @Test
    public void moveDownContinuosMultipleSelection() {
        Integer[] indices = new Integer[] { 1, 2 };
        MoveType.DOWN.move(indices, items, -1);
        assertThat(items).containsExactly(first, fourth, second, third);
    }

    @Test
    public void moveDownMultipleSelection() {
        Integer[] indices = new Integer[] { 0, 2 };
        SelectionAndFocus selection = MoveType.DOWN.move(indices, items, 0);
        assertThat(items).containsExactly(second, first, fourth, third);
        assertEquals(1, selection.getFocus());
        assertEquals(3, selection.getRow());
        assertArrayEquals(new int[] { 1 }, selection.getRows());
    }

    @Test
    public void moveDownSingleSelection() {
        Integer[] indices = new Integer[] { 1 };
        MoveType.DOWN.move(indices, items, -1);
        assertThat(items).containsExactly(first, third, second, fourth);
    }
}
