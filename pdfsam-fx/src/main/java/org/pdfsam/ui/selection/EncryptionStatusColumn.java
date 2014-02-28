/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/feb/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.util.Comparator;

import javafx.beans.value.ObservableValue;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.Tooltip;
import javafx.util.Callback;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.pdf.EncryptionStatus;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Definition of the {@link EncryptionStatus} column of the selection table
 * 
 * @author Andrea Vacondio
 *
 */
public enum EncryptionStatusColumn implements SelectionTableColumn<EncryptionStatus> {
    STATUS {

        public String getColumnTitle() {
            return null;
        }

        @Override
        public ObservableValue<EncryptionStatus> getObservableValue(SelectionTableRowData data) {
            return data.getDocumentDescriptor().encryptionStatusProperty();
        }

        @Override
        public String getTextValue(EncryptionStatus item) {
            return (item != null) ? item.name() : "";
        }

        @Override
        public Callback<TableColumn<SelectionTableRowData, EncryptionStatus>, TableCell<SelectionTableRowData, EncryptionStatus>> cellFactory() {
            return new Callback<TableColumn<SelectionTableRowData, EncryptionStatus>, TableCell<SelectionTableRowData, EncryptionStatus>>() {
                public TableCell<SelectionTableRowData, EncryptionStatus> call(
                        TableColumn<SelectionTableRowData, EncryptionStatus> param) {
                    return new TableCell<SelectionTableRowData, EncryptionStatus>() {
                        {
                            setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
                            setText("");
                        }

                        @Override
                        public void updateItem(final EncryptionStatus item, boolean empty) {
                            super.updateItem(item, empty);
                            if (item != null) {
                                switch (item) {
                                case ENCRYPTED:
                                    setGraphic(AwesomeDude.createIconLabel(AwesomeIcon.LOCK));
                                    setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                                            "This document is encrypted, double click to provide a password.")));
                                    break;
                                case DECRYPTED_WITH_USER_PWD:
                                    setGraphic(AwesomeDude.createIconLabel(AwesomeIcon.UNLOCK));
                                    AwesomeDude.setIcon(this, AwesomeIcon.UNLOCK);
                                    break;
                                default:
                                    setGraphic(null);
                                    setTooltip(null);
                                    break;
                                }
                            } else {
                                setGraphic(null);
                                setTooltip(null);
                            }

                        }

                    };
                }
            };
        }

        public TableColumn<SelectionTableRowData, EncryptionStatus> getTableColumn() {
            TableColumn<SelectionTableRowData, EncryptionStatus> tableColumn = new TableColumn<>(getColumnTitle());
            tableColumn.setCellFactory(cellFactory());
            tableColumn.setCellValueFactory(cellValueFactory());
            tableColumn.setSortable(false);
            tableColumn.setMaxWidth(22);
            tableColumn.setMinWidth(22);

            return tableColumn;
        }

        public Comparator<EncryptionStatus> comparator() {
            // not used
            return null;
        }
    };
}
