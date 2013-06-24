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
enum LongColumn implements SelectionTableColumn<Long> {
    SIZE {
        public String getColumnName() {
            return DefaultI18nContext.getInstance().i18n("Size");
        }

        public Long getValueFor(SelectionTableRowData rowData, int rowNum) {
            return rowData.getDocumentDescriptor().getFile().length();
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
    },
    LAST_MODIFIED {
        private FastDateFormat formatter = FastDateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG);

        public String getColumnName() {
            return DefaultI18nContext.getInstance().i18n("Modified");
        }

        public Long getValueFor(SelectionTableRowData rowData, int rowNum) {
            return rowData.getDocumentDescriptor().getFile().lastModified();
        }

        @Override
        public TableCellRenderer getRenderer() {
            return new BaseSelectionTableCellRenderer() {

                @Override
                String getStringValue(Object value) {
                    if (value != null) {
                        return formatter.format(((Long) value).longValue());
                    }
                    return EMPTY;
                }
            };
        }
    };

    public Class<Long> getColumnClass() {
        return Long.class;
    }

}
