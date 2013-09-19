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
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.bushe.swing.event.annotation.ReferenceStrength;
import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.WithEventNamespace;
import org.pdfsam.pdf.PdfLoadCompletedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.bushe.swing.event.EventBus.publish;
import static org.pdfsam.gui.view.selection.SelectionChangedEvent.selectionChanged;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * {@link JTable} configured to display pdf selection
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTable extends JTable implements WithEventNamespace {

    private static final Logger LOG = LoggerFactory.getLogger(SelectionTable.class);

    private static final int ROW_HEADER_WIDTH = 30;
    private static final int ROW_HEIGHT = 22;
    private EventNamespace namespace = EventNamespace.NULL;
    private SelectionTableRowSorter sorter;

    public SelectionTable(SelectionTableModel dm, EventNamespace namespace) {
        super(dm);
        this.namespace = namespace;
        sorter = new SelectionTableRowSorter(dm);
        sorter.addRowSorterListener(this);
        setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        setFillsViewportHeight(true);
        putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);
        setRowHeight(ROW_HEIGHT);
        setBackground(Color.WHITE);
        setBorder(BorderFactory.createEmptyBorder());
        setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        getSelectionModel().addListSelectionListener(new SelectionListener());
        AnnotationProcessor.process(this);
        AnnotationProcessor.process(new TableSelectionController(dm));
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
    public SelectionTableRowSorter getRowSorter() {
        return sorter;
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

    @EventSubscriber
    public void onRemoveSelected(RemoveSelectedEvent event) {
        if (event.getNamespace().isParentOf(getEventNamespace())) {
            ((SelectionTableModel) getModel()).deleteIndexes(getSelectedRows());
        }
    }

    @EventSubscriber
    public void onMoveSelected(MoveSelectedEvent event) {
        if (event.getNamespace().isParentOf(getEventNamespace())) {
            sorter.unsort();
            int[] selected = getSelectedRows();
            if (event.getType() == MoveType.DOWN) {
                ((SelectionTableModel) getModel()).moveDownIndexes(selected);
                setRowSelectionInterval(selected[0] + 1, selected[selected.length - 1] + 1);
            } else {
                ((SelectionTableModel) getModel()).moveUpIndexes(selected);
                setRowSelectionInterval(selected[0] - 1, selected[selected.length - 1] - 1);
            }
        }
    }

    @EventSubscriber
    public void onSort(SortRequestEvent event) {
        if (event.getNamespace().isParentOf(getEventNamespace())) {
            sorter.sort();
            resizeAndRepaint();
        }
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

        @EventSubscriber(priority = Integer.MAX_VALUE)
        public void onRemoveSelected(RemoveSelectedEvent event) {
            if (event.getNamespace().isParentOf(getEventNamespace())) {
                int formerRows = numberOfRows;
                numberOfRows = SelectionTable.this.getRowCount();
                fireTableRowsDeleted(numberOfRows, formerRows - 1);
            }
        }
    }

    /**
     * Selection listeners notifying of changes in the table selection.
     * 
     * @author Andrea Vacondio
     * 
     */
    private class SelectionListener implements ListSelectionListener {
        public void valueChanged(ListSelectionEvent event) {
            if (!event.getValueIsAdjusting()) {
                ListSelectionModel lsm = (ListSelectionModel) event.getSource();
                if (lsm.isSelectionEmpty()) {
                    publish(selectionChanged(namespace).clearSelection());
                    LOG.trace("Selection cleared for {}", namespace);
                } else {
                    SelectionChangedEvent newSelectionEvent = selectionChanged(namespace)
                            .startSelectionAt(lsm.getMinSelectionIndex()).endSelectionAt(lsm.getMaxSelectionIndex())
                            .ofTotalRows(getModel().getRowCount());

                    publish(newSelectionEvent);
                    LOG.trace("{} for {}", newSelectionEvent, namespace);
                }
            }
        }
    }

    /**
     * Controller for the table selection. Caches and restores the selection when possible.
     * 
     * @author Andrea Vacondio
     * 
     */
    private class TableSelectionController {
        private SelectionTableRowData selected;
        private SelectionTableModel model;

        public TableSelectionController(SelectionTableModel model) {
            requireNotNull(model, "Model cannot be null");
            this.model = model;
        }

        @EventSubscriber(referenceStrength = ReferenceStrength.STRONG)
        public void onBeforeSort(BeforeSortEvent event) {
            if (event.getNamespace().isParentOf(getEventNamespace())) {
                selected = null;
                ListSelectionModel selectionModel = getSelectionModel();
                if (!selectionModel.isSelectionEmpty()
                        && selectionModel.getMinSelectionIndex() == selectionModel.getMaxSelectionIndex()) {
                    selected = model.getRow(selectionModel.getMinSelectionIndex());
                }
                selectionModel.clearSelection();
            }
        }

        @EventSubscriber(referenceStrength = ReferenceStrength.STRONG)
        public void onAfterSort(AfterSortEvent event) {
            if (event.getNamespace().isParentOf(getEventNamespace()) && selected != null) {
                ListSelectionModel selectionModel = getSelectionModel();
                int index = model.getRowIndex(selected);
                selectionModel.setSelectionInterval(index, index);
                selected = null;
            }
        }
    }

    /**
     * Listen for model changes and takes appropriate actions if the table is sorted
     * 
     * @author Andrea Vacondio
     * 
     */
    // TODO remove
    private class ModelChangeListener implements TableModelListener {
        public void tableChanged(TableModelEvent e) {
            if (!sorter.isUnsorted() && e.getType() != TableModelEvent.DELETE) {
                if (isInsert(e) || isUpdateOnSortedColumn(e)) {
                    sorter.sort();
                    resizeAndRepaint();
                }
            }
        }

        private boolean isInsert(TableModelEvent e) {
            return e.getType() == TableModelEvent.INSERT;
        }

        private boolean isUpdateOnSortedColumn(TableModelEvent e) {
            return (e.getType() == TableModelEvent.UPDATE && (e.getColumn() == TableModelEvent.ALL_COLUMNS || sorter
                    .isSortedOnColumn(e.getColumn())));
        }
    }

}
