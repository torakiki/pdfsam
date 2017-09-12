/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 06/ago/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class MultipleSelectionAndFocusTest {

    @Test
    public void moveUp() {
        MultipleSelectionAndFocus victim = new MultipleSelectionAndFocus(-1);
        victim.moveUp(4);
        victim.moveUp(5);
        assertEquals(-1, victim.getFocus());
        assertEquals(3, victim.getRow());
        assertArrayEquals(new int[] { 4 }, victim.getRows());
    }

    @Test
    public void moveUpWithFocus() {
        MultipleSelectionAndFocus victim = new MultipleSelectionAndFocus(4);
        victim.moveUp(4);
        victim.moveUp(5);
        assertEquals(3, victim.getFocus());
    }

    @Test
    public void moveDown() {
        MultipleSelectionAndFocus victim = new MultipleSelectionAndFocus(-1);
        victim.moveDown(6);
        victim.moveDown(5);
        assertEquals(-1, victim.getFocus());
        assertEquals(7, victim.getRow());
        assertArrayEquals(new int[] { 6 }, victim.getRows());
    }

    @Test
    public void moveDownWithFocus() {
        MultipleSelectionAndFocus victim = new MultipleSelectionAndFocus(5);
        victim.moveDown(4);
        victim.moveDown(5);
        assertEquals(6, victim.getFocus());
    }
}
