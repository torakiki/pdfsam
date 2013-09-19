/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/lug/2013
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.RowSorter;
import javax.swing.SortOrder;

import org.apache.commons.lang3.ObjectUtils;
import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.pdfsam.support.RequireUtils;

import static org.pdfsam.support.RequireUtils.require;

/**
 * A Row Sorter largely inspired by the DefaultRowSorter and the TableRowSorter. It doesn't provide filter functionalities and it performs sorting directly on the Model.
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTableRowSorter extends RowSorter<SelectionTableModel> {

    /**
     * Whether or not we resort on TableModelEvent.UPDATEs.
     */
    private boolean sortsOnUpdates;

    private SortKey sortKey = null;

    private SelectionTableModel model;

    /**
     * Creates an empty <code>DefaultRowSorter</code>.
     */
    public SelectionTableRowSorter(SelectionTableModel model) {
        RequireUtils.requireNotNull(model, "Model cannot be null");
        this.model = model;
        AnnotationProcessor.process(this);
    }

    /**
     * @return the underlying model
     */
    @Override
    public final SelectionTableModel getModel() {
        return model;
    }

    /**
     * Sets the sort keys. This creates a copy of the supplied {@code List}; subsequent changes to the supplied {@code List} do not effect this {@code DefaultRowSorter}. If the
     * sort keys have changed this triggers a sort.
     * 
     * @param sortKeys
     *            the new <code>SortKeys</code>; <code>null</code> is a shorthand for specifying an empty list, indicating that the view should be unsorted
     * @throws IllegalArgumentException
     *             if any of the values in <code>sortKeys</code> are null or have a column index outside the range of the model
     */
    @Override
    public void setSortKeys(List<? extends SortKey> sortKeys) {
        require(sortKeys != null && sortKeys.size() < 2, "SelectionableRowSorter supports only one sort key");
        SortKey old = this.sortKey;
        this.sortKey = null;
        if (sortKeys.size() == 1) {
            SortKey key = sortKeys.get(0);
            require(isValidKey(key), "Invalid sort key");
            this.sortKey = key;
        }
        if (ObjectUtils.notEqual(old, sortKey)) {
            fireSortOrderChanged();
            sort();
        }
    }

    private boolean isValidKey(SortKey key) {
        return (key != null && key.getColumn() >= 0 && key.getColumn() < model.getColumnCount());
    }

    /**
     * Returns the current sort keys. This returns an unmodifiable {@code non-null List}. If you need to change the sort keys, make a copy of the returned {@code List}, mutate the
     * copy and invoke {@code setSortKeys} with the new list.
     * 
     * @return the current sort order
     */
    @Override
    public List<? extends SortKey> getSortKeys() {
        List<SortKey> ret = new ArrayList<>();
        if (hasSortKey()) {
            ret.add(sortKey);
        }
        return Collections.unmodifiableList(ret);
    }

    /**
     * If true, specifies that a sort should happen when the underlying model is updated (<code>rowsUpdated</code> is invoked). For example, if this is true and the user edits an
     * entry the location of that item in the view may change. The default is false.
     * 
     * @param sortsOnUpdates
     *            whether or not to sort on update events
     */
    public void setSortsOnUpdates(boolean sortsOnUpdates) {
        this.sortsOnUpdates = sortsOnUpdates;
    }

    /**
     * Returns true if a sort should happen when the underlying model is updated; otherwise, returns false.
     * 
     * @return whether or not to sort when the model is updated
     */
    public boolean getSortsOnUpdates() {
        return sortsOnUpdates;
    }

    /**
     * Reverses the sort order from ascending to descending (or descending to ascending) if the specified column is already the primary sorted column; otherwise, makes the
     * specified column the primary sorted column, with an ascending sort order. If the specified column is not sortable, this method has no effect.
     * 
     * @param column
     *            index of the column to make the primary sorted column, in terms of the underlying model
     * @throws IndexOutOfBoundsException
     *             {@inheritDoc}
     * @see #setSortable(int,boolean)
     * @see #setMaxSortKeys(int)
     */
    @Override
    public void toggleSortOrder(int column) {
        checkColumn(column);
        List<SortKey> keys = new ArrayList<>();
        if (hasSortKey() && sortKey.getColumn() == column) {
            keys.add(toggleSortKey(sortKey));
        } else {
            keys.add(new SortKey(column, SortOrder.ASCENDING));
        }
        setSortKeys(keys);
    }

    private boolean hasSortKey() {
        return sortKey != null;
    }

    private SortKey toggleSortKey(SortKey key) {
        if (key.getSortOrder() == SortOrder.ASCENDING) {
            return new SortKey(key.getColumn(), SortOrder.DESCENDING);
        }
        return new SortKey(key.getColumn(), SortOrder.ASCENDING);
    }

    @Override
    public int convertRowIndexToView(int index) {
        return index;
    }

    @Override
    public int convertRowIndexToModel(int index) {
        return index;
    }

    public boolean isUnsorted() {
        return !hasSortKey() || sortKey.getSortOrder() == SortOrder.UNSORTED;
    }

    public void sort() {
        if (!isUnsorted()) {
            model.sort(sortKey);
        }
    }

    /**
     * @param column
     * @return true if it's sorted on the given column
     */
    public boolean isSortedOnColumn(int column) {
        return (hasSortKey() && sortKey.getColumn() == column);
    }

    public void unsort() {
        setSortKeys(Collections.EMPTY_LIST);
    }

    @Override
    public int getViewRowCount() {
        return model.getRowCount();
    }

    @Override
    public int getModelRowCount() {
        return model.getRowCount();
    }

    @Override
    public void modelStructureChanged() {
        unsort();
    }

    @Override
    public void allRowsChanged() {
        sort();
    }

    @Override
    public void rowsInserted(int firstRow, int endRow) {
        if (hasSortKey()) {
            sort();
        }
    }

    @Override
    public void rowsUpdated(int firstRow, int endRow) {
        sort();
    }

    @Override
    public void rowsUpdated(int firstRow, int endRow, int column) {
        checkColumn(column);
        if (hasSortKey() && sortKey.getColumn() == column) {
            rowsUpdated(firstRow, endRow);
        }
    }

    @Override
    public void rowsDeleted(int firstRow, int endRow) {
        // nothing to do
    }

    private void checkColumn(int column) {
        if (column < 0 || column >= model.getColumnCount()) {
            throw new IndexOutOfBoundsException("column beyond range of TableModel");
        }
    }
}
