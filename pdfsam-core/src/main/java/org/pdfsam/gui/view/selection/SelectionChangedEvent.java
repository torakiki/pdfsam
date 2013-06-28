/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/giu/2013
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

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.pdfsam.gui.event.BaseEvent;
import org.pdfsam.gui.event.EventNamespace;

import static org.pdfsam.support.RequireUtils.require;

/**
 * Event sent when the selection on the selection table changed
 * 
 * @author Andrea Vacondio
 * 
 */
final class SelectionChangedEvent extends BaseEvent {

    private static final int UNSELECTED = -1;

    private int startIndex = UNSELECTED;
    private int endIndex = UNSELECTED;
    private int totalRows = 0;

    private SelectionChangedEvent(EventNamespace namespace, int startIndex, int endIndex) {
        super(namespace);
        this.startIndex = startIndex;
        this.endIndex = endIndex;
    }

    /**
     * @return true the selection has been cleared
     */
    public boolean isClearSelection() {
        return startIndex == UNSELECTED || endIndex == UNSELECTED;
    }

    /**
     * 
     * @return true if its a single row selection event
     */
    public boolean isSingleSelection() {
        return !isClearSelection() && endIndex == startIndex;
    }

    public boolean canMove(MoveType type) {
        if (type == MoveType.DOWN) {
            return !isClearSelection() && endIndex < totalRows - 1;
        }
        return !isClearSelection() && startIndex > 0;
    }

    public int getTotalRows() {
        return totalRows;
    }

    public int getStartIndex() {
        return startIndex;
    }

    public int getEndIndex() {
        return endIndex;
    }

    /**
     * @return the event where the selection has been cleared
     */
    public SelectionChangedEvent clearSelection() {
        this.startIndex = UNSELECTED;
        this.endIndex = UNSELECTED;
        this.totalRows = 0;
        return this;
    }

    /**
     * @param index
     * @return the event where a single selection has been set
     */
    public SelectionChangedEvent select(int index) {
        require(index >= 0, "Unable to select negative index");
        this.startIndex = index;
        this.endIndex = index;
        return this;
    }

    /**
     * @param index
     * @return the event where a selection starts at the given index
     */
    public SelectionChangedEvent startSelectionAt(int index) {
        require(index >= 0, "Unable to select negative index");
        this.startIndex = index;
        return this;
    }

    /**
     * @param index
     * @return the event where a selection ends at the given index
     */
    public SelectionChangedEvent endSelectionAt(int index) {
        require(index >= 0, "Unable to select negative index");
        require(startIndex <= index, "Selection cannot end before start");
        this.endIndex = index;
        return this;
    }

    /**
     * @param totalNumberOfRows
     * @return the event where the total number of rows available has been set
     */
    public SelectionChangedEvent ofTotalRows(int totalNumberOfRows) {
        require(totalNumberOfRows >= 0, "Cannot select rows if no row is available");
        this.totalRows = totalNumberOfRows;
        return this;
    }

    /**
     * @param namespace
     * @return a new instance of an event notifying that the selection has been cleared
     */
    public static SelectionChangedEvent selectionChanged(EventNamespace namespace) {
        return new SelectionChangedEvent(namespace, UNSELECTED, UNSELECTED);
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("from", startIndex).append("to", endIndex).append("of", totalRows)
                .toString();
    }

}
