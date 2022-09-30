/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/ott/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import javafx.beans.value.ObservableValue;
import org.apache.commons.lang3.StringUtils;

import java.util.Comparator;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Definition of the {@link Integer} columns of the selection table
 * 
 * @author Andrea Vacondio
 *
 */
public enum IntColumn implements SelectionTableColumn<Integer> {
    PAGES {
        @Override
        public String getColumnTitle() {
            return i18n().tr("Pages");
        }

        @Override
        public ObservableValue<Integer> getObservableValue(SelectionTableRowData data) {
            return data.descriptor().pages();
        }

        @Override
        public String getTextValue(Integer item) {
            if (item != null && item.intValue() > 0) {
                return item.toString();
            }
            return StringUtils.EMPTY;
        }

        @Override
        public Comparator<Integer> comparator() {
            return Comparator.naturalOrder();
        }
    };
}
