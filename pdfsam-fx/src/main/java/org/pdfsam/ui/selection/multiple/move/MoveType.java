/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/giu/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.util.Arrays;
import java.util.Collections;

import javafx.collections.ObservableList;

import org.apache.commons.lang3.ArrayUtils;
import org.pdfsam.pdf.PdfDocumentDescriptor;

/**
 * Types of moves for the selected items in the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public enum MoveType {
    TOP {
        @Override
        public <T extends PdfDocumentDescriptor> SelectionAndFocus move(Integer[] selected, ObservableList<T> items,
                int focused) {
            if (isSingleSelection(selected, items) && isNotFirst(selected)) {
                T item = items.remove(selected[0].intValue());
                items.add(0, item);
                return new SingleSelectionAndFocus(0);
            }
            return SelectionAndFocus.NULL;
        }
    },
    UP {
        @Override
        public <T extends PdfDocumentDescriptor> SelectionAndFocus move(Integer[] selected, ObservableList<T> items,
                int focused) {
            if (isSubselection(selected, items)) {
                MultipleSelectionAndFocus newSelection = new MultipleSelectionAndFocus(focused);
                Arrays.parallelSort(selected);
                if (isNotFirst(selected)) {
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
        public <T extends PdfDocumentDescriptor> SelectionAndFocus move(Integer[] selected, ObservableList<T> items,
                int focused) {
            if (isSubselection(selected, items)) {
                MultipleSelectionAndFocus newSelection = new MultipleSelectionAndFocus(focused);
                Arrays.parallelSort(selected, Collections.reverseOrder(Integer::compare));
                if (isNotLast(selected, items)) {
                    Arrays.stream(selected).forEach((i) -> {
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
        public <T extends PdfDocumentDescriptor> SelectionAndFocus move(Integer[] selected, ObservableList<T> items,
                int focused) {
            if (isSingleSelection(selected, items) && isNotLast(selected, items)) {
                T item = items.remove(selected[0].intValue());
                items.add(items.size(), item);
                return new SingleSelectionAndFocus(items.size() - 1);
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
    public abstract <T extends PdfDocumentDescriptor> SelectionAndFocus move(Integer[] indicesToMove,
            ObservableList<T> items, int focused);

}
