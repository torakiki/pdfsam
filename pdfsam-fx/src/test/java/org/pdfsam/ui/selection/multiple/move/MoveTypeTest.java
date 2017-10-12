/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 06/ago/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
package org.pdfsam.ui.selection.multiple.move;

import static org.hamcrest.Matchers.contains;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.pdfsam.pdf.PdfDocumentDescriptor;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(MockitoJUnitRunner.class)
public class MoveTypeTest {

    @Mock
    private PdfDocumentDescriptor first;
    @Mock
    private PdfDocumentDescriptor second;
    @Mock
    private PdfDocumentDescriptor third;
    @Mock
    private PdfDocumentDescriptor fourth;
    private ObservableList<PdfDocumentDescriptor> items;

    @Before
    public void setUp() {
        items = FXCollections.observableArrayList(first, second, third, fourth);
    }

    @Test
    public void moveTopFirst() {
        Integer[] indices = new Integer[] { 0 };
        assertEquals(SelectionAndFocus.NULL, MoveType.TOP.move(indices, items, -1));
    }

    @Test
    public void moveTopMultipleSelection() {
        Integer[] indices = new Integer[] { 0, 1 };
        assertEquals(SelectionAndFocus.NULL, MoveType.TOP.move(indices, items, -1));
    }

    @Test
    public void moveTop() {
        Integer[] indices = new Integer[] { 1 };
        MoveType.TOP.move(indices, items, -1);
        assertEquals(second, items.get(0));
    }

    @Test
    public void moveBottomLast() {
        Integer[] indices = new Integer[] { 3 };
        assertEquals(SelectionAndFocus.NULL, MoveType.BOTTOM.move(indices, items, -1));
    }

    @Test
    public void moveBottomMultipleSelection() {
        Integer[] indices = new Integer[] { 0, 1 };
        assertEquals(SelectionAndFocus.NULL, MoveType.BOTTOM.move(indices, items, -1));
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
        assertThat(items, contains(second, third, first, fourth));
    }

    @Test
    public void moveUpMultipleSelection() {
        Integer[] indices = new Integer[] { 1, 3 };
        MoveType.UP.move(indices, items, -1);
        assertThat(items, contains(second, first, fourth, third));
    }

    @Test
    public void moveUpSingleSelection() {
        Integer[] indices = new Integer[] { 2 };
        MoveType.UP.move(indices, items, -1);
        assertThat(items, contains(first, third, second, fourth));
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
        assertThat(items, contains(first, fourth, second, third));
    }

    @Test
    public void moveDownMultipleSelection() {
        Integer[] indices = new Integer[] { 0, 2 };
        MoveType.DOWN.move(indices, items, -1);
        assertThat(items, contains(second, first, fourth, third));
    }

    @Test
    public void moveDownSingleSelection() {
        Integer[] indices = new Integer[] { 1 };
        MoveType.DOWN.move(indices, items, -1);
        assertThat(items, contains(first, third, second, fourth));
    }
}
