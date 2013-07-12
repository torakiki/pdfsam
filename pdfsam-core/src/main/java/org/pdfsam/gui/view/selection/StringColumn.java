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

import javax.swing.table.TableCellRenderer;

import org.pdfsam.context.DefaultI18nContext;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;

/**
 * Definition of the {@link String} columns of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
enum StringColumn implements SelectionTableColumn<String> {

    PAGE_SELECTION {
        public String getColumnName() {
            return DefaultI18nContext.getInstance().i18n("Page ranges");
        }

        public String getValueFor(SelectionTableRowData data) {
            return data.getPageSelection();
        }

    };

    private TableCellRenderer stringCellRenderer = new BaseSelectionTableCellRenderer() {

        @Override
        String getStringValue(Object value) {
            return defaultString((String) value, EMPTY);
        }
    };

    public Class<String> getColumnClass() {
        return String.class;
    }

    public TableCellRenderer getRenderer() {
        return stringCellRenderer;
    }
}
