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

import java.awt.Component;
import java.io.File;

import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

import org.pdfsam.context.DefaultI18nContext;

import static org.apache.commons.lang3.StringUtils.EMPTY;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Definition of the {@link File} columns of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
enum FileColumn implements SelectionTableColumn<File> {

    NAME {
        public String getColumnName() {
            return DefaultI18nContext.getInstance().i18n("Name");
        }

        @Override
        public TableCellRenderer getRenderer() {
            return new BaseSelectionTableCellRenderer() {
                @Override
                public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
                        boolean hasFocus, int row, int column) {
                    super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                    setToolTipText(((File) value).getAbsolutePath());
                    return this;
                }

                @Override
                String getStringValue(Object value) {
                    if (value != null) {
                        return ((File) value).getName();
                    }
                    return EMPTY;
                }
            };
        }
    };

    public File getValueFor(SelectionTableRowData rowData) {
        requireNotNull(rowData.getDocumentDescriptor().getFile(), "File cannt be null");
        return rowData.getDocumentDescriptor().getFile();
    }

    public Class<File> getColumnClass() {
        return File.class;
    }

    public int compare(SelectionTableRowData o1, SelectionTableRowData o2) {
        return (getValueFor(o1).getName().toLowerCase().compareTo(getValueFor(o2).getName().toLowerCase()));
    }

}
