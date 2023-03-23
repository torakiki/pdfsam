/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23 ott 2017
 * Copyright 2017 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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

import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.util.Callback;
import org.pdfsam.model.ObservableAtomicReference;

/**
 * Index column showing a row header with the row index
 * 
 * @author Andrea Vacondio
 *
 */
public class IndexColumn extends TableColumn<SelectionTableRowData, Object> {

    public IndexColumn() {
        super("#");
        setComparator(null);
        setSortable(false);
        setPrefWidth(15);
        setCellFactory(new Callback<>() {

            @Override
            public TableCell<SelectionTableRowData, Object> call(TableColumn<SelectionTableRowData, Object> param) {
                return new TableCell<>() {
                    @Override
                    public void updateItem(Object item, boolean empty) {
                        super.updateItem(item, empty);
                        if (empty) {
                            setText("");
                        } else {
                            setText(Integer.toString(getIndex() + 1));
                        }
                    }
                };
            }
        });

        setCellValueFactory(param -> new ObservableAtomicReference<>(new Object()));
    }
}
