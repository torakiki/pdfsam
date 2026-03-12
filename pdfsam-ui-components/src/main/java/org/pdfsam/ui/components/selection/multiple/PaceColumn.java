/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30 ago 2016
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

import javafx.beans.value.ObservableValue;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.util.Callback;

import java.util.Comparator;
import java.util.Objects;

import static org.apache.commons.lang3.StringUtils.defaultIfBlank;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Definition of the pace column of the selection table
 *
 * @author Andrea Vacondio
 */
public class PaceColumn implements SelectionTableColumn<String> {

    public PaceColumn() {
        // nothing
    }

    @Override
    public String getColumnTitle() {
        return i18n().tr("Pace");
    }

    @Override
    public ObservableValue<String> getObservableValue(SelectionTableRowData data) {
        return data.pace;
    }

    @Override
    public String getTextValue(String item) {
        return Objects.toString(item, "1");
    }

    @Override
    public Comparator<String> comparator() {
        return Comparator.naturalOrder();
    }

    @Override
    public Float prefWidth() {
        return 50F;
    }

    @Override
    public TableColumn<SelectionTableRowData, String> getTableColumn() {
        TableColumn<SelectionTableRowData, String> tableColumn = SelectionTableColumn.super.getTableColumn();
        tableColumn.setEditable(true);
        tableColumn.setOnEditCommit(t -> t.getTableView().getItems().get(t.getTablePosition().getRow()).pace.set(
                defaultIfBlank(t.getNewValue(), "1")));
        return tableColumn;
    }

    @Override
    public Callback<TableColumn<SelectionTableRowData, String>, TableCell<SelectionTableRowData, String>> cellFactory() {
        return param -> new TooltippedTextFieldTableCell(
                i18n().tr("Double click to set the number of pages after which the task will switch to the next file"));
    }

}
