/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/giu/2013
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
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;

import org.bushe.swing.event.EventBus;
import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.WithEventNamespace;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadCompletedEvent;

import static org.apache.commons.lang3.ArrayUtils.isNotEmpty;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Table model for the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
class SelectionTableModel extends AbstractTableModel implements WithEventNamespace {

    private List<SelectionTableRowData> data = new ArrayList<>();
    private List<SelectionTableColumn<?>> columns = new ArrayList<>();
    private EventNamespace namespace;

    public SelectionTableModel(EventNamespace eventNamespace, SelectionTableColumn<?>... columns) {
        requireNotNull(eventNamespace, "Event namespace cannot be null");
        this.namespace = eventNamespace;
        for (SelectionTableColumn<?> current : columns) {
            this.columns.add(current);
        }
        AnnotationProcessor.process(this);
    }

    public int getRowCount() {
        return data.size();
    }

    public int getColumnCount() {
        return columns.size();
    }

    public Object getValueAt(int row, int col) {
        return columns.get(col).getValueFor(data.get(row));
    }

    @Override
    public Class<?> getColumnClass(int col) {
        return columns.get(col).getColumnClass();
    }

    @Override
    public String getColumnName(int col) {
        return columns.get(col).getColumnName();
    }

    SelectionTableRowData getRow(int row) {
        if (data.size() > row) {
            return data.get(row);
        }
        return null;
    }

    int getRowIndex(SelectionTableRowData row) {
        return data.indexOf(row);
    }

    public TableCellRenderer getColumnRenderer(int col) {
        return columns.get(col).getRenderer();
    }

    public EventNamespace getEventNamespace() {
        return namespace;
    }

    public void deleteIndexes(int[] toDelete) {
        Collection<SelectionTableRowData> toRemove = getRows(toDelete);
        if (data.removeAll(toRemove)) {
            fireTableDataChanged();
        }
    }

    public void moveUpIndexes(int[] toMove) {
        if (isNotEmpty(toMove) && toMove.length < data.size() && toMove[0] > 0) {
            Collections.rotate(data.subList(toMove[0] - 1, toMove[toMove.length - 1] + 1), -1);
            fireTableRowsUpdated(toMove[0] - 1, toMove[toMove.length - 1]);
        }
    }

    public void moveDownIndexes(int[] toMove) {
        if (isNotEmpty(toMove) && toMove.length < data.size() && toMove[toMove.length - 1] < data.size() - 1) {
            Collections.rotate(data.subList(toMove[0], toMove[toMove.length - 1] + 2), 1);
            fireTableRowsUpdated(toMove[0], toMove[toMove.length - 1] + 1);
        }
    }

    private Collection<SelectionTableRowData> getRows(int[] indexes) {
        List<SelectionTableRowData> retList = new ArrayList<>();
        for (int current : indexes) {
            retList.add(data.get(current));
        }
        return retList;
    }

    @EventSubscriber
    public void onLoadDocumentsCompletion(PdfLoadCompletedEvent event) {
        if (event.getNamespace().isParentOf(getEventNamespace())) {
            for (PdfDocumentDescriptor current : event.getDocuments()) {
                data.add(new SelectionTableRowData(current));
            }
            fireRowsAdded(event.getDocuments().size());
        }
    }

    @EventSubscriber
    public void onClear(ClearSelectionTableEvent event) {
        if (event.getNamespace().isParentOf(getEventNamespace())) {
            data.clear();
            fireTableDataChanged();
        }
    }

    private void fireRowsAdded(int rows) {
        if (rows > 0) {
            fireTableRowsInserted(data.size() - rows, data.size() - 1);
        }
        EventBus.publish(new SortRequestEvent(namespace));
    }

    public void sort(final SortKey sorting) {
        if (sorting.getSortOrder() != SortOrder.UNSORTED) {
            EventBus.publish(new BeforeSortEvent(namespace));
            Collections.sort(data, new Comparator<SelectionTableRowData>() {
                @Override
                public int compare(SelectionTableRowData o1, SelectionTableRowData o2) {
                    SelectionTableColumn<? extends Comparable> column = columns.get(sorting.getColumn());
                    int retVal = column.getValueFor(o1).compareTo(column.getValueFor(o2));
                    return SortOrder.ASCENDING == sorting.getSortOrder() ? retVal : -retVal;
                }
            });
            EventBus.publish(new AfterSortEvent(namespace));
        }
    }
}
