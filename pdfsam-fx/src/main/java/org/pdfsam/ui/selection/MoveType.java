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
package org.pdfsam.ui.selection;

import static org.pdfsam.support.RequireUtils.require;

import java.util.Collections;

import javafx.collections.ObservableList;

/**
 * Types of moves for the selected items in the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public enum MoveType {
    UP {
        @Override
        public Interval move(ObservableList<Integer> toMove, ObservableList<?> items) {
            if (!toMove.isEmpty() && toMove.size() < items.size() && toMove.get(0) > 0) {
                Collections.rotate(items.subList(toMove.get(0) - 1, toMove.get(toMove.size() - 1) + 1), -1);
                return new Interval(toMove.get(0) - 1, toMove.get(toMove.size() - 1));
            }
            return Interval.NULL;
        }
    },
    DOWN {
        @Override
        public Interval move(ObservableList<Integer> toMove, ObservableList<?> items) {
            if (!toMove.isEmpty() && toMove.size() < items.size() && toMove.get(toMove.size() - 1) < items.size() - 1) {
                Collections.rotate(items.subList(toMove.get(0), toMove.get(toMove.size() - 1) + 2), 1);
                return new Interval(toMove.get(0) + 1, toMove.get(toMove.size() - 1) + 2);
            }
            return Interval.NULL;
        }
    };

    /**
     * Moves the given contiguous collection of indices (a interval) in the given collection if items
     * 
     * @param toMove
     * @param items
     * @return a new interval of the given toMove items with the position of the interval after the move took place
     */
    public abstract Interval move(ObservableList<Integer> toMove, ObservableList<?> items);

    /**
     * A single interval of selected rows where start is inclusive, end is exclusive.
     * 
     * @author Andrea Vacondio
     * 
     */
    public static class Interval {
        public static final Interval NULL = new Interval(0, 0);
        private int start, end;

        private Interval(int start, int end) {
            require(start <= end, "Interval cannot end before start");
            this.start = start;
            this.end = end;
        }

        public int getStart() {
            return start;
        }

        public int getEnd() {
            return end;
        }

    }
}
