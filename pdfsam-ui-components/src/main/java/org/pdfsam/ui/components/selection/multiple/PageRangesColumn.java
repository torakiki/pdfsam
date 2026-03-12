/*
 * This file is part of the PDF Split And Merge source code
 * Created on 27/nov/2013
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

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isNoneBlank;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Definition of the page ranges column of the selection table
 *
 * @author Andrea Vacondio
 */
public class PageRangesColumn implements SelectionTableColumn<String> {
    private String tooltipMessage = i18n().tr("Set selected pages (ex: 2 or 5-23 or 2,5-7,12-)");

    public PageRangesColumn() {
        // nothing
    }

    public PageRangesColumn(String tooltipMessage) {
        if (isNoneBlank(tooltipMessage)) {
            this.tooltipMessage = tooltipMessage;
        }
    }

    @Override
    public String getColumnTitle() {
        return i18n().tr("Page ranges");
    }

    @Override
    public ObservableValue<String> getObservableValue(SelectionTableRowData data) {
        return data.pageSelection;
    }

    @Override
    public String getTextValue(String item) {
        return Objects.toString(item, EMPTY);
    }

    @Override
    public Comparator<String> comparator() {
        return Comparator.naturalOrder();
    }

    @Override
    public TableColumn<SelectionTableRowData, String> getTableColumn() {
        TableColumn<SelectionTableRowData, String> tableColumn = SelectionTableColumn.super.getTableColumn();
        tableColumn.setEditable(true);
        tableColumn.setOnEditCommit(
                t -> t.getTableView().getItems().get(t.getTablePosition().getRow()).pageSelection.set(t.getNewValue()));
        return tableColumn;
    }

    @Override
    public Callback<TableColumn<SelectionTableRowData, String>, TableCell<SelectionTableRowData, String>> cellFactory() {
        return param -> new TooltippedTextFieldTableCell(tooltipMessage);
    }
}
