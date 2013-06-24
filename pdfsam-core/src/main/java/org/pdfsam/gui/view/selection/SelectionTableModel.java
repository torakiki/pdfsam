/*
 * Created on 15/giu/2013
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.gui.view.selection;

import java.util.ArrayList;
import java.util.List;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.WithEventNamespace;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadCompletedEvent;

import static org.pdfsam.support.RequireUtils.require;

/**
 * Table model for the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTableModel extends AbstractTableModel implements WithEventNamespace {

    private List<SelectionTableRowData> data = new ArrayList<SelectionTableRowData>();
    private List<SelectionTableColumn<?>> columns = new ArrayList<SelectionTableColumn<?>>();
    private EventNamespace eventNamespace;

    public SelectionTableModel(EventNamespace eventNamespace, SelectionTableColumn<?>... columns) {
        require(eventNamespace != null, "Event namespace cannot be null");
        this.eventNamespace = eventNamespace;
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
        return columns.get(col).getValueFor(data.get(row), row);
    }

    @Override
    public Class<?> getColumnClass(int col) {
        return columns.get(col).getColumnClass();
    }

    @Override
    public String getColumnName(int col) {
        return columns.get(col).getColumnName();
    }

    public TableCellRenderer getColumnRenderer(int col) {
        return columns.get(col).getRenderer();
    }

    public EventNamespace getEventNamespace() {
        return eventNamespace;
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
    }

}
