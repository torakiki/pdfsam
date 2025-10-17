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

import javafx.scene.control.TableColumn;
import javafx.scene.control.cell.CheckBoxTableCell;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Provides a reverse column with an editable checkbox
 * 
 * @author Andrea Vacondio
 *
 */
public class ReverseColumn implements TableColumnProvider<Boolean> {

    public String getColumnTitle() {
        return i18n().tr("Reverse");
    }

    @Override
    public TableColumn<SelectionTableRowData, Boolean> getTableColumn() {
        TableColumn<SelectionTableRowData, Boolean> tableColumn = new TableColumn<>(getColumnTitle());
        tableColumn.setCellFactory(CheckBoxTableCell.forTableColumn(tableColumn));
        tableColumn.setCellValueFactory(param -> {
            if (param.getValue() != null) {
                return param.getValue().reverse;
            }
            return null;
        });
        tableColumn.setMaxWidth(1000F);
        return tableColumn;
    }

}
