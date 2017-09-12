/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/feb/2014
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


/**
 * Holds information about selection and focus
 * 
 * @author Andrea Vacondio
 *
 */
public interface SelectionAndFocus {
    SelectionAndFocus NULL = new SelectionAndFocus() {

        @Override
        public int[] getRows() {
            return new int[0];
        }

        @Override
        public int getRow() {
            return -1;
        }

        @Override
        public int getFocus() {
            return -1;
        }
    };

    /**
     * @return the index of item with focus
     */
    int getFocus();

    /**
     * @return the index of selected item
     */
    int getRow();

    /**
     * @return the index of additional selected items in case of multi selection
     */
    int[] getRows();
}
