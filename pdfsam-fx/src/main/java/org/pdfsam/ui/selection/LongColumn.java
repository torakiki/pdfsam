/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.selection;

import java.text.DateFormat;
import java.util.Comparator;

import javafx.beans.property.SimpleLongProperty;
import javafx.beans.value.ObservableValue;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.pdfsam.context.DefaultI18nContext;

/**
 * Definition of the {@link Long} columns of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public enum LongColumn implements SelectionTableColumn<Number> {

    SIZE {
        public String getColumnTitle() {
            return DefaultI18nContext.getInstance().i18n("Size");
        }

        @Override
        public ObservableValue<Number> getObservableValue(SelectionTableRowData data) {
            return new SimpleLongProperty(data.getDocumentDescriptor().getFile().length());
        }

        @Override
        public String getTextValue(Number item) {
            return (item != null) ? FileUtils.byteCountToDisplaySize(item.longValue()) : "";
        }

        public Comparator<Number> comparator() {
            return LONG_COMPARATOR;
        }
    },
    LAST_MODIFIED {
        private FastDateFormat formatter = FastDateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.MEDIUM);

        public String getColumnTitle() {
            return DefaultI18nContext.getInstance().i18n("Modified");
        }

        @Override
        public ObservableValue<Number> getObservableValue(SelectionTableRowData data) {
            return new SimpleLongProperty(data.getDocumentDescriptor().getFile().lastModified());
        }

        @Override
        public String getTextValue(Number item) {
            return (item != null) ? formatter.format(item) : "";
        }

        public Comparator<Number> comparator() {
            return LONG_COMPARATOR;
        }
    },
    PAGES {
        public String getColumnTitle() {
            return DefaultI18nContext.getInstance().i18n("Pages");
        }

        @Override
        public ObservableValue<Number> getObservableValue(SelectionTableRowData data) {
            return data.getDocumentDescriptor().pagesPropery();
        }

        @Override
        public String getTextValue(Number item) {
            if (item != null && item.intValue() > 0) {
                return ObjectUtils.toString(item);
            }
            return StringUtils.EMPTY;
        }

        public Comparator<Number> comparator() {
            return INT_COMPARATOR;
        }
    };

    private static final Comparator<Number> LONG_COMPARATOR = new Comparator<Number>() {
        public int compare(Number o1, Number o2) {
            return Long.valueOf(o1.longValue()).compareTo(Long.valueOf(o2.longValue()));
        }
    };

    private static final Comparator<Number> INT_COMPARATOR = new Comparator<Number>() {
        public int compare(Number o1, Number o2) {
            return Integer.valueOf(o1.intValue()).compareTo(Integer.valueOf(o2.intValue()));
        }
    };

}
