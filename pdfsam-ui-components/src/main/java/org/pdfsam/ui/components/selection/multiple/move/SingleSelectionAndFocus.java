/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/feb/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import static org.sejda.commons.util.RequireUtils.requireNotNegative;

/**
 * Single selection where the item selected is the one with focus
 *
 * @author Andrea Vacondio
 */
record SingleSelectionAndFocus(int row) implements SelectionAndFocus {

    SingleSelectionAndFocus {
        requireNotNegative(row);
    }

    @Override
    public int getFocus() {
        return row;
    }

    /**
     * @return an empty array since this is a single row selection
     */
    @Override
    public int[] getRows() {
        return new int[0];
    }

}
