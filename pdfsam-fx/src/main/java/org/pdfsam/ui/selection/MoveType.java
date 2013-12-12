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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javafx.collections.ObservableList;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

/**
 * Types of moves for the selected items in the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public enum MoveType {
    UP {
        @Override
        public SelectionAndFocus move(Integer[] selected, ObservableList<?> items, int focused) {
            if (isSubselection(selected, items)) {
                SelectionAndFocus newSelection = new SelectionAndFocus(focused);
                Arrays.parallelSort(selected);
                if (selected[0] > 0) {
                    Arrays.stream(selected).forEach((i) -> {
                        Collections.swap(items, i, i - 1);
                        newSelection.moveUp(i);
                    });
                    return newSelection;
                }
            }
            return SelectionAndFocus.NULL;
        }
    },
    DOWN {
        @Override
        public SelectionAndFocus move(Integer[] selected, ObservableList<?> items, int focused) {
            if (isSubselection(selected, items)) {
                SelectionAndFocus newSelection = new SelectionAndFocus(focused);
                Arrays.parallelSort(selected);
                if (selected[selected.length - 1] < items.size() - 1) {
                    Arrays.stream(selected).forEach((i) -> {
                        Collections.swap(items, i, i + 1);
                        newSelection.moveDown(i);
                    });
                    return newSelection;
                }
            }
            return SelectionAndFocus.NULL;
        }
    };
    boolean isSubselection(Integer[] toMove, ObservableList<?> items) {
        return !ArrayUtils.isEmpty(toMove) && toMove.length < items.size();
    }


    /**
     * Moves the given collection of indices in the given collection if items
     * 
     * @param indicesToMove
     * @param items
     * @param focused
     *            the index of the focused item
     * @return a new SelectionAndFocus holding the new coordinates for focus and selection
     */
    public abstract SelectionAndFocus move(Integer[] indicesToMove, ObservableList<?> items, int focused);

    public static final class SelectionAndFocus {
        public static final SelectionAndFocus NULL = new SelectionAndFocus(-1);
        private int focus = -1;
        private int originalFocus = -1;
        private int row = -1;
        private Set<Integer> rows = new HashSet<>();

        private SelectionAndFocus(int originalFocus) {
            this.originalFocus = originalFocus;
        }

        private void move(int row, int newRow) {
            if (focus == -1) {
                if (originalFocus == row) {
                    focus = newRow;
                }
            }
            if (this.row == -1) {
                this.row = newRow;
            } else {
                rows.add(newRow);
            }
        }

        private void moveUp(int row) {
            int newRow = row - 1;
            move(row, newRow);
        }

        private void moveDown(int row) {
            int newRow = row + 1;
            move(row, newRow);
        }

        public int getFocus() {
            return focus;
        }

        public int getRow() {
            return row;
        }

        public int[] getRows() {
            // TODO this sucks
            return ArrayUtils.toPrimitive(rows.toArray(new Integer[rows.size()]));
        }

        @Override
        public String toString() {
            return ReflectionToStringBuilder.toString(this);
        }
    }
}
