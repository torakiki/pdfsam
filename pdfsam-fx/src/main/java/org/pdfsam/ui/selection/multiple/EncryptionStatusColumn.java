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
package org.pdfsam.ui.selection.multiple;

import static org.apache.commons.lang3.StringUtils.defaultString;

import java.util.Comparator;

import javafx.beans.value.ObservableValue;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.util.Callback;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.EncryptionStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfDocumentDescriptorProvider;
import org.pdfsam.ui.selection.EncryptionStatusIndicator;

/**
 * Definition of the {@link EncryptionStatus} column of the selection table
 * 
 * @author Andrea Vacondio
 *
 */
public class EncryptionStatusColumn implements SelectionTableColumn<EncryptionStatus>, ModuleOwned {
    private String ownerModule = StringUtils.EMPTY;

    public EncryptionStatusColumn(String ownerModule) {
        this.ownerModule = defaultString(ownerModule);
    }

    public String getOwnerModule() {
        return ownerModule;
    }

    public String getColumnTitle() {
        return null;
    }

    @Override
    public ObservableValue<EncryptionStatus> getObservableValue(SelectionTableRowData data) {
        return data.getPdfDocumentDescriptor().encryptionStatusProperty();
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
                return new EncryptionStatusTableCell();
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

    private class EncryptionStatusTableCell extends TableCell<SelectionTableRowData, EncryptionStatus> implements
            PdfDocumentDescriptorProvider {
        private EncryptionStatusIndicator encryptionStatus = null;

        private EncryptionStatusTableCell() {
            setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
            setText("");
            encryptionStatus = new EncryptionStatusIndicator(this, EncryptionStatusColumn.this.getOwnerModule());

        }

        public PdfDocumentDescriptor getPdfDocumentDescriptor() {
            return ((SelectionTableRowData) getTableRow().getItem()).getPdfDocumentDescriptor();
        }

        @Override
        public void updateItem(final EncryptionStatus item, boolean empty) {
            super.updateItem(item, empty);
            if (item != null) {
                encryptionStatus.updateEncryptionStatus(item);
                setGraphic(encryptionStatus);
            } else {
                setGraphic(null);
            }
        }
    }
}
