/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 27/nov/2013
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

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;

import java.util.Comparator;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TextField;
import javafx.util.Callback;

import org.pdfsam.context.DefaultI18nContext;

/**
 * Definition of the {@link String} columns of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
enum StringColumn implements SelectionTableColumn<String> {
    PAGE_SELECTION {
        public String getColumnTitle() {
            return DefaultI18nContext.getInstance().i18n("Page ranges");
        }

        @Override
        public ObservableValue<String> getObservableValue(SelectionTableRowData data) {
            return new SimpleStringProperty(data.getPageSelection());
        }

        @Override
        public String getTextValue(String item) {
            return defaultString(item, EMPTY);
        }

        public Comparator<String> comparator() {
            return new Comparator<String>() {
                public int compare(String o1, String o2) {
                    return o1.compareTo(o2);
                }
            };
        }

        @Override
        public TableColumn<SelectionTableRowData, String> getTableColumn() {
            TableColumn<SelectionTableRowData, String> tableColumn = super.getTableColumn();
            tableColumn.setEditable(true);
            // TODO find out why hitting enter to cancel the edit before committing
            tableColumn.setOnEditCommit(t -> {
                System.out.println("commit " + t);
                t.getTableView().getItems().get(t.getTablePosition().getRow()).setPageSelection(t.getNewValue());
            });

            return tableColumn;
        }

        /**
         * @return the editable cell factory used to create the {@link TableCell}
         */
        public Callback<TableColumn<SelectionTableRowData, String>, TableCell<SelectionTableRowData, String>> cellFactory() {
            return new Callback<TableColumn<SelectionTableRowData, String>, TableCell<SelectionTableRowData, String>>() {
                public TableCell<SelectionTableRowData, String> call(TableColumn<SelectionTableRowData, String> param) {
                    return new TableCell<SelectionTableRowData, String>() {
                        private TextField textField = new TextField();
                        {
                            setEditable(true);
                            textField.focusedProperty().addListener((o, old, n) -> {
                                if (!n) {
                                    System.out.println("editing " + isEditing());
                                    commitEdit(textField.getText());
                                }
                            });
                        }

                        @Override
                        public void startEdit() {
                            if (!isEmpty()) {
                                super.startEdit();
                                textField.setText(getTextValue(getItem()));
                                textField.setMinWidth(this.getWidth() - this.getGraphicTextGap() * 2);
                                setText(null);
                                setGraphic(textField);
                                textField.selectAll();
                                textField.requestFocus();
                            }
                        }

                        @Override
                        public void cancelEdit() {
                            super.cancelEdit();
                            setText(getItem());
                            setGraphic(null);
                        }

                        @Override
                        public void updateItem(String item, boolean empty) {
                            super.updateItem(item, empty);
                            if (empty) {
                                setText(null);
                                setGraphic(null);
                            } else {
                                if (isEditing()) {
                                    textField.setText(getTextValue(item));
                                    setText(null);
                                    setGraphic(textField);
                                } else {
                                    setText(getTextValue(item));
                                    setGraphic(null);
                                }
                            }
                        }
                    };
                }
            };
        }
    }
}
