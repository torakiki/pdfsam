/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/giu/2013
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
package org.pdfsam.ui.selection.multiple.move;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;

import org.apache.commons.lang3.ArrayUtils;
import org.pdfsam.core.support.KeyValueItem;

import javafx.collections.ObservableList;

/**
 * Types of moves for the selected items in the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public enum MoveType {
    TOP {
        @Override
        public <T> SelectionAndFocus move(Integer[] selected, ObservableList<T> items, int focused) {
            if (isSubselection(selected, items)) {
                MultipleSelectionAndFocus newSelection = new MultipleSelectionAndFocus(focused);
                Arrays.parallelSort(selected, Collections.reverseOrder(Integer::compare));
                if (isNotFirst(selected)) {
                    LinkedList<IntKeyValueItem<T>> toMove = new LinkedList<>();
                    Arrays.stream(selected)
                            .forEach(i -> toMove.push(new IntKeyValueItem<>(i, items.remove(i.intValue()))));
                    int addIdx = 0;
                    for (IntKeyValueItem<T> current : toMove) {
                        items.add(addIdx, current.getValue());
                        newSelection.moveTo(current.getKey(), addIdx);
                        addIdx++;
                    }
                    return newSelection;
                }
            }
            return SelectionAndFocus.NULL;
        }
    },
    UP {
        @Override
        public <T> SelectionAndFocus move(Integer[] selected, ObservableList<T> items, int focused) {
            if (isSubselection(selected, items)) {
                MultipleSelectionAndFocus newSelection = new MultipleSelectionAndFocus(focused);
                Arrays.parallelSort(selected);
                if (isNotFirst(selected)) {
                    Arrays.stream(selected).forEach(i -> {
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
        public <T> SelectionAndFocus move(Integer[] selected, ObservableList<T> items, int focused) {
            if (isSubselection(selected, items)) {
                MultipleSelectionAndFocus newSelection = new MultipleSelectionAndFocus(focused);
                Arrays.parallelSort(selected, Collections.reverseOrder(Integer::compare));
                if (isNotLast(selected, items)) {
                    Arrays.stream(selected).forEach(i -> {
                        Collections.swap(items, i, i + 1);
                        newSelection.moveDown(i);
                    });
                    return newSelection;
                }
            }
            return SelectionAndFocus.NULL;
        }
    },
    BOTTOM {
        @Override
        public <T> SelectionAndFocus move(Integer[] selected, ObservableList<T> items, int focused) {
            if (isSubselection(selected, items)) {
                MultipleSelectionAndFocus newSelection = new MultipleSelectionAndFocus(focused);
                Arrays.parallelSort(selected, Collections.reverseOrder(Integer::compare));
                if (isNotLast(selected, items)) {
                    LinkedList<IntKeyValueItem<T>> toMove = new LinkedList<>();
                    Arrays.stream(selected)
                            .forEach(i -> toMove.push(new IntKeyValueItem<>(i, items.remove(i.intValue()))));
                    toMove.stream().forEach(i -> {
                        items.add(i.getValue());
                        newSelection.moveTo(i.getKey(), items.size() - 1);
                    });
                    return newSelection;
                }
            }
            return SelectionAndFocus.NULL;
        }
    };

    boolean isNotFirst(Integer[] selected) {
        return selected[0] > 0;
    }

    boolean isNotLast(Integer[] selected, ObservableList<?> items) {
        return selected[0] < items.size() - 1;
    }

    boolean isSubselection(Integer[] toMove, ObservableList<?> items) {
        return !ArrayUtils.isEmpty(toMove) && toMove.length < items.size();
    }

    boolean isSingleSelection(Integer[] toMove, ObservableList<?> items) {
        return !ArrayUtils.isEmpty(toMove) && toMove.length == 1 && items.size() > 1;
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
    public abstract <T> SelectionAndFocus move(Integer[] indicesToMove, ObservableList<T> items, int focused);

    /**
     * Helper class to hold information about the original index of a row
     * 
     * @author Andrea Vacondio
     *
     * @param <V>
     */
    private static class IntKeyValueItem<V> implements KeyValueItem<Integer, V> {

        private Integer key;
        private V value;

        private IntKeyValueItem(Integer key, V value) {
            requireNotNullArg(key, "Key cannot be null");
            this.key = key;
            this.value = value;
        }

        @Override
        public Integer getKey() {
            return key;
        }

        @Override
        public V getValue() {
            return value;
        }

    }

}
