/*
 * This file is part of the PDF Split And Merge source code
 * Created on 26/nov/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.selection.multiple;

import javafx.beans.property.SimpleLongProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.util.Callback;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.FastDateFormat;

import java.text.DateFormat;
import java.util.Comparator;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Definition of the {@link Long} columns of the selection table
 *
 * @author Andrea Vacondio
 */
public enum LongColumn implements SelectionTableColumn<Number> {

    SIZE {
        @Override
        public String getColumnTitle() {
            return i18n().tr("Size");
        }

        @Override
        public ObservableValue<Number> getObservableValue(SelectionTableRowData data) {
            return new SimpleLongProperty(data.descriptor().getFile().length());
        }

        @Override
        public String getTextValue(Number item) {
            return (item != null) ? FileUtils.byteCountToDisplaySize(item.longValue()) : "";
        }

        @Override
        public Comparator<Number> comparator() {
            return Comparator.comparingLong(Number::longValue);
        }

        @Override
        public Callback<TableColumn<SelectionTableRowData, Number>, TableCell<SelectionTableRowData, Number>> cellFactory() {
            return param -> new TableCell<>() {
                @Override
                public void updateItem(final Number item, boolean empty) {
                    super.updateItem(item, empty);
                    if (empty || item == null) {
                        setText("");
                        setAccessibleText(null);
                    } else {
                        var text = getTextValue(item);
                        setText(text);
                        setAccessibleText(i18n().tr("Size: {0}", text));
                    }
                }
            };
        }

        @Override
        public Float prefWidth() {
            return 50F;
        }
    },
    LAST_MODIFIED {
        @Override
        public String getColumnTitle() {
            return i18n().tr("Modified");
        }

        @Override
        public ObservableValue<Number> getObservableValue(SelectionTableRowData data) {
            return new SimpleLongProperty(data.descriptor().getFile().lastModified());
        }

        @Override
        public String getTextValue(Number item) {
            return (item != null) ? FORMATTER.format(item) : "";
        }

        @Override
        public Comparator<Number> comparator() {
            return Comparator.comparingLong(Number::longValue);
        }

        @Override
        public Callback<TableColumn<SelectionTableRowData, Number>, TableCell<SelectionTableRowData, Number>> cellFactory() {
            return param -> new TableCell<>() {
                @Override
                public void updateItem(final Number item, boolean empty) {
                    super.updateItem(item, empty);
                    if (empty || item == null) {
                        setText("");
                        setAccessibleText(null);
                    } else {
                        var text = getTextValue(item);
                        setText(text);
                        setAccessibleText(i18n().tr("Modified: {0}", text));
                    }
                }
            };
        }

        @Override
        public Float prefWidth() {
            return 110F;
        }
    };

    private static final FastDateFormat FORMATTER = FastDateFormat.getDateTimeInstance(DateFormat.LONG,
            DateFormat.MEDIUM);

}
