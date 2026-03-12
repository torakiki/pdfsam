/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/12/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@soberlemur.com).
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
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.util.Callback;
import org.apache.commons.lang3.StringUtils;

import java.util.Comparator;

import static java.util.Objects.nonNull;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Column showing the number of selected pages. This goes with those tools that allow a page range column where the user can select a subset of pages.
 *
 * @author Andrea Vacondio
 */
public class SelectedPagesColumn implements SelectionTableColumn<Integer> {

    public SelectedPagesColumn() {
        // nothing
    }

    @Override
    public String getColumnTitle() {
        return i18n().tr("Selected pages");
    }

    @Override
    public ObservableValue<Integer> getObservableValue(SelectionTableRowData data) {
        return data.selectedPages.asObject();
    }

    @Override
    public String getTextValue(Integer item) {
        if (nonNull(item) && item >= 0) {
            return item.toString();
        }
        return StringUtils.EMPTY;
    }

    @Override
    public Float prefWidth() {
        return 50F;
    }

    @Override
    public Comparator<Integer> comparator() {
        return Comparator.naturalOrder();
    }

    @Override
    public Callback<TableColumn<SelectionTableRowData, Integer>, TableCell<SelectionTableRowData, Integer>> cellFactory() {
        return param -> new TooltippedTableCell<>(i18n().tr("Total number of selected pages")) {
            @Override
            void onUpdateItem(Integer item, boolean empty) {
                if (empty || item == null) {
                    setText("");
                    setAccessibleText(null);
                } else {
                    var pages = getTextValue(item);
                    setText(pages);
                    setAccessibleText(i18n().tr("{0} selected pages", pages));
                }
            }
        };
    }

}
