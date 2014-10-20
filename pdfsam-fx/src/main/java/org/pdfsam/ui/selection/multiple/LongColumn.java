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
package org.pdfsam.ui.selection.multiple;

import java.text.DateFormat;
import java.util.Comparator;

import javafx.beans.property.SimpleLongProperty;
import javafx.beans.value.ObservableValue;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.pdfsam.i18n.DefaultI18nContext;

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
            return new SimpleLongProperty(data.getFile().length());
        }

        @Override
        public String getTextValue(Number item) {
            return (item != null) ? FileUtils.byteCountToDisplaySize(item.longValue()) : "";
        }

        public Comparator<Number> comparator() {
            return Comparator.comparingLong(Number::longValue);
        }
    },
    LAST_MODIFIED {

        public String getColumnTitle() {
            return DefaultI18nContext.getInstance().i18n("Modified");
        }

        @Override
        public ObservableValue<Number> getObservableValue(SelectionTableRowData data) {
            return new SimpleLongProperty(data.getFile().lastModified());
        }

        @Override
        public String getTextValue(Number item) {
            return (item != null) ? FORMATTER.format(item) : "";
        }

        public Comparator<Number> comparator() {
            return Comparator.comparingLong(Number::longValue);
        }
    };

    private static final FastDateFormat FORMATTER = FastDateFormat.getDateTimeInstance(DateFormat.LONG,
            DateFormat.MEDIUM);

}
