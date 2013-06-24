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

import javax.swing.table.TableCellRenderer;

import org.apache.commons.lang3.ObjectUtils;
import org.pdfsam.context.DefaultI18nContext;

/**
 * Definition of the {@link Integer} columns of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public enum IntegerColumn implements SelectionTableColumn<Integer> {
    COUNTER {
        public String getColumnName() {
            return "#";
        }

        public Integer getValueFor(SelectionTableRowData rowData, int rowNum) {
            return rowNum + 1;
        }
    },
    PAGES {
        public String getColumnName() {
            return DefaultI18nContext.getInstance().i18n("Pages");
        }

        public Integer getValueFor(SelectionTableRowData rowData, int rowNum) {
            return rowData.getDocumentDescriptor().getPages();
        }

    };

    public TableCellRenderer getRenderer() {
        return new BaseSelectionTableCellRenderer() {

            @Override
            String getStringValue(Object value) {
                return ObjectUtils.toString(value);
            }
        };
    }

    public Class<Integer> getColumnClass() {
        return Integer.class;
    }

}
