/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/giu/2013
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

import static org.sejda.commons.util.RequireUtils.requireArg;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;
import static org.sejda.commons.util.RequireUtils.requireState;

import java.util.Collection;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.pdfsam.ui.selection.multiple.move.MoveType;

/**
 * Event sent when the selection on the selection table changed
 * 
 * @author Andrea Vacondio
 * 
 */
final class SelectionChangedEvent {

    private int top = Integer.MAX_VALUE;
    private int bottom = -1;
    private int totalRows = 0;

    private SelectionChangedEvent(Collection<? extends Integer> selected) {
        requireNotNullArg(selected, "Input selection cannot be null");
        selected.forEach(i -> {
            bottom = Math.max(i, bottom);
            top = Math.min(i, top);
        });
    }

    private SelectionChangedEvent() {
        // nothing
    }

    /**
     * @return true the selection has been cleared
     */
    public boolean isClearSelection() {
        return top == Integer.MAX_VALUE && bottom == -1;
    }

    /**
     * @return true if its a single row selection event
     */
    public boolean isSingleSelection() {
        return !isClearSelection() && top == bottom;
    }

    /**
     * @return the index for the single selection
     * @throws IllegalStateException
     *             if the event is not a single selection
     */
    public int getSingleSelection() {
        requireState(isSingleSelection(), "Single selection expected");
        return top;
    }

    public boolean canMove(MoveType type) {
        if (isClearSelection()) {
            return false;
        }
        switch (type) {
        case BOTTOM:
            return isSingleSelection() && bottom < totalRows - 1;
        case DOWN:
            return bottom < totalRows - 1;
        case TOP:
            return isSingleSelection() && top > 0;
        default:
            return top > 0;
        }
    }

    public int getTotalRows() {
        return totalRows;
    }

    /**
     * @return the event where the selection has been cleared
     */
    public static SelectionChangedEvent clearSelectionEvent() {
        return new SelectionChangedEvent();
    }

    /**
     * @param index
     * @return the event where a single selection has been set
     */
    public static SelectionChangedEvent select(Collection<? extends Integer> index) {
        return new SelectionChangedEvent(index);
    }

    /**
     * @param totalNumberOfRows
     * @return the event where the total number of rows available has been set
     */
    public SelectionChangedEvent ofTotalRows(int totalNumberOfRows) {
        requireArg(totalNumberOfRows >= 0, "Cannot select rows if no row is available");
        this.totalRows = totalNumberOfRows;
        return this;
    }

    @Override
    public String toString() {
        return ReflectionToStringBuilder.toString(this);
    }
}
