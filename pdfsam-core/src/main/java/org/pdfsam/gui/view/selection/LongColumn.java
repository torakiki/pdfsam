/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/lug/2013
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

import java.text.DateFormat;

import javax.swing.table.TableCellRenderer;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.pdfsam.context.DefaultI18nContext;

import static org.apache.commons.lang3.StringUtils.EMPTY;

/**
 * Definition of the {@link Long} columns of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public enum LongColumn implements SelectionTableColumn<Long> {

    SIZE {
        public String getColumnName() {
            return DefaultI18nContext.getInstance().i18n("Size");
        }

        @Override
        public TableCellRenderer getRenderer() {
            return new BaseSelectionTableCellRenderer() {

                @Override
                String getStringValue(Object value) {
                    if (value != null) {
                        return FileUtils.byteCountToDisplaySize((Long) value);
                    }
                    return EMPTY;
                }
            };
        }

        @Override
        public Long getValueFor(SelectionTableRowData rowData) {
            return rowData.getDocumentDescriptor().getFile().length();
        }
    },
    LAST_MODIFIED {
        private FastDateFormat formatter = FastDateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG);

        public String getColumnName() {
            return DefaultI18nContext.getInstance().i18n("Modified");
        }

        @Override
        public TableCellRenderer getRenderer() {
            return new BaseSelectionTableCellRenderer() {

                @Override
                String getStringValue(Object value) {
                    if (value != null) {
                        return formatter.format(value);
                    }
                    return EMPTY;
                }
            };
        }

        @Override
        public Long getValueFor(SelectionTableRowData rowData) {
            return rowData.getDocumentDescriptor().getFile().lastModified();
        }
    };

    public Class<Long> getColumnClass() {
        return Long.class;
    }
}
