/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30 ago 2016
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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

import org.pdfsam.i18n.DefaultI18nContext;

import javafx.beans.value.ObservableValue;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.scene.control.cell.CheckBoxTableCell;
import javafx.util.Callback;

/**
 * Provides a reverse column with an editable checkbox
 * 
 * @author Andrea Vacondio
 *
 */
public class ReverseColumn implements TableColumnProvider<Boolean> {

    public String getColumnTitle() {
        return DefaultI18nContext.getInstance().i18n("Reverse");
    }

    @Override
    public TableColumn<SelectionTableRowData, Boolean> getTableColumn() {
        TableColumn<SelectionTableRowData, Boolean> tableColumn = new TableColumn<>(getColumnTitle());
        tableColumn.setCellFactory(CheckBoxTableCell.forTableColumn(tableColumn));
        tableColumn.setCellValueFactory(
                new Callback<CellDataFeatures<SelectionTableRowData, Boolean>, ObservableValue<Boolean>>() {
                    @Override
                    public ObservableValue<Boolean> call(CellDataFeatures<SelectionTableRowData, Boolean> param) {
                        if (param.getValue() != null) {
                            return param.getValue().reverse;
                        }
                        return null;
                    }
                });
        return tableColumn;
    }

}
