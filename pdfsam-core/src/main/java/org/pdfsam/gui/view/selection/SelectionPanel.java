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

import java.awt.BorderLayout;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.UIManager;
import javax.swing.table.TableColumn;

import org.pdfsam.gui.event.String;
import org.pdfsam.gui.event.ModuleOwned;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Panel that lets the user select input documents
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionPanel extends JPanel implements ModuleOwned {

    private String namespace = String.NULL;
    private SelectionTable selectionTable;
    private SelectionTableModel tableModel;

    public SelectionPanel(String namespace) {
        super(new BorderLayout());
        requireNotNull(namespace, "Event namespace cannot be null");
        this.namespace = namespace;
        add(new SelectionTableToolbar(namespace), BorderLayout.PAGE_START);

        tableModel = new SelectionTableModel(namespace, new SelectionTableColumn<?>[] { FileColumn.NAME,
                IntegerColumn.PAGES, LongColumn.SIZE, StringColumn.PAGE_SELECTION, LongColumn.LAST_MODIFIED });
        selectionTable = new SelectionTable(tableModel, namespace);

        selectionTable.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        for (int i = 0; i < selectionTable.getColumnCount(); i++) {
            TableColumn column = selectionTable.getColumnModel().getColumn(i);
            column.setCellRenderer(tableModel.getColumnRenderer(i));
        }

        JScrollPane scrollPane = new JScrollPane(selectionTable);
        scrollPane.setBorder(BorderFactory.createLineBorder(UIManager.getColor("MenuItem.selectionBackground"), 1));
        JTable header = selectionTable.getRowHeader();
        JViewport jv = new JViewport();
        jv.setView(header);
        jv.setPreferredSize(header.getMaximumSize());
        scrollPane.setRowHeader(jv);
        scrollPane.setCorner(ScrollPaneConstants.UPPER_LEFT_CORNER, header.getTableHeader());
        add(scrollPane, BorderLayout.CENTER);
    }

    public String getOwnerModule() {
        return namespace;
    }

}
