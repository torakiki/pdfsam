/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/giu/2013
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

import java.awt.Color;
import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.WithEventNamespace;
import org.pdfsam.pdf.PdfLoadCompletedEvent;

/**
 * {@link JTable} configured to display pdf selection
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTable extends JTable implements WithEventNamespace {

    /**
     * 
     */
    private static final int ROW_HEADER_WIDTH = 30;
    /**
     * 
     */
    private static final int ROW_HEIGHT = 22;
    private EventNamespace namespace = EventNamespace.NULL;

    public SelectionTable(TableModel dm, EventNamespace namespace) {
        super(dm);
        this.namespace = namespace;
        setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        setAutoCreateRowSorter(true);
        setFillsViewportHeight(true);
        putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);
        setRowHeight(ROW_HEIGHT);
        setBackground(Color.WHITE);
        setBorder(BorderFactory.createEmptyBorder());
    }

    public JTable getRowHeader() {
        JTable rowHeader = new JTable(new RowHeaderModel());
        setRowHeight(ROW_HEIGHT);
        rowHeader.setColumnSelectionAllowed(false);
        rowHeader.setShowGrid(false);
        rowHeader.setFocusable(false);
        rowHeader.setRowHeight(ROW_HEIGHT);
        rowHeader.setBackground(Color.WHITE);
        rowHeader.setColumnSelectionAllowed(false);
        rowHeader.setCellSelectionEnabled(false);
        rowHeader.setFillsViewportHeight(true);
        rowHeader.setDefaultRenderer(String.class, getTableHeader().getDefaultRenderer());
        rowHeader.getColumnModel().getColumn(0).setMaxWidth(ROW_HEADER_WIDTH);
        return rowHeader;
    }

    @Override
    public Component prepareRenderer(TableCellRenderer renderer, int row, int column) {
        Component component = super.prepareRenderer(renderer, row, column);
        int rendererWidth = component.getPreferredSize().width;
        TableColumn tableColumn = getColumnModel().getColumn(column);
        tableColumn.setPreferredWidth(Math.max(rendererWidth + getIntercellSpacing().width,
                tableColumn.getPreferredWidth()));
        return component;
    }

    @Override
    public boolean getScrollableTracksViewportWidth() {
        return getPreferredSize().width < getParent().getWidth();
    }

    public EventNamespace getEventNamespace() {
        return namespace;
    }

    /**
     * Model for the model of the {@link JTable} used as header of the table.
     * 
     * @author Andrea Vacondio
     * 
     */
    public class RowHeaderModel extends AbstractTableModel {
        public RowHeaderModel() {
            AnnotationProcessor.process(this);
        }

        private int numberOfRows = 0;

        public int getRowCount() {
            return numberOfRows;
        }

        public int getColumnCount() {
            return 1;
        }

        public Object getValueAt(int row, int col) {
            return Integer.toString(row + 1);
        }

        @Override
        public Class<?> getColumnClass(int col) {
            return String.class;
        }

        @Override
        public String getColumnName(int col) {
            return "";
        }

        @EventSubscriber
        public void onLoadDocumentsCompletion(PdfLoadCompletedEvent event) {
            if (event.getNamespace().isParentOf(getEventNamespace())) {
                int rows = event.getDocuments().size();
                if (rows > 0) {
                    numberOfRows += rows;
                    fireTableRowsInserted(numberOfRows - rows, numberOfRows - 1);
                }
            }
        }

        @EventSubscriber
        public void onClear(ClearSelectionTableEvent event) {
            if (event.getNamespace().isParentOf(getEventNamespace())) {
                numberOfRows = 0;
                fireTableDataChanged();
            }
        }
    }

}
