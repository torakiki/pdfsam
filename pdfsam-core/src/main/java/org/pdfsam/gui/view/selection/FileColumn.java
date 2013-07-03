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
import org.pdfsam.gui.view.selection.FileColumn.ComparableFileWrapper;
import org.pdfsam.support.RequireUtils;

import static org.apache.commons.lang3.StringUtils.EMPTY;

/**
 * Definition of the {@link File} columns of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
enum FileColumn implements SelectionTableColumn<ComparableFileWrapper> {

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
                    setToolTipText(((ComparableFileWrapper) value).getFile().getAbsolutePath());
                    return this;
                }

                @Override
                String getStringValue(Object value) {
                    if (value != null) {
                        return ((ComparableFileWrapper) value).getFile().getName();
                    }
                    return EMPTY;
                }
            };
        }
    };

    public ComparableFileWrapper getValueFor(SelectionTableRowData rowData, int rowNum) {
        return new ComparableFileWrapper(rowData.getDocumentDescriptor().getFile());
    }

    public Class<ComparableFileWrapper> getColumnClass() {
        return ComparableFileWrapper.class;
    }

    /**
     * Wrapper around a File instance that can nicely used by the table sorter
     * 
     * @author Andrea Vacondio
     * 
     */
    public static final class ComparableFileWrapper implements Comparable<ComparableFileWrapper> {
        private final File wrapped;

        private ComparableFileWrapper(File file) {
            RequireUtils.require(file != null, "File cannt be null");
            this.wrapped = file;
        }

        public File getFile() {
            return wrapped;
        }

        @Override
        public int compareTo(ComparableFileWrapper o) {
            return this.getFile().getName().toLowerCase().compareTo(o.getFile().getName().toLowerCase());
        }

    }
}
