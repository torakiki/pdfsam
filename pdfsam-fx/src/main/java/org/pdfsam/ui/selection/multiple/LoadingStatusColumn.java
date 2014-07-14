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
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfDocumentDescriptorProvider;
import org.pdfsam.ui.selection.LoadingStatusIndicator;

/**
 * Definition of the {@link PdfDescriptorLoadingStatus} column of the selection table
 * 
 * @author Andrea Vacondio
 *
 */
public class LoadingStatusColumn implements SelectionTableColumn<PdfDescriptorLoadingStatus>, ModuleOwned {
    private String ownerModule = StringUtils.EMPTY;

    public LoadingStatusColumn(String ownerModule) {
        this.ownerModule = defaultString(ownerModule);
    }

    public String getOwnerModule() {
        return ownerModule;
    }

    public String getColumnTitle() {
        return null;
    }

    @Override
    public ObservableValue<PdfDescriptorLoadingStatus> getObservableValue(SelectionTableRowData data) {
        return data.loadedProperty();
    }

    @Override
    public String getTextValue(PdfDescriptorLoadingStatus item) {
        return (item != null) ? item.name() : "";
    }

    @Override
    public Callback<TableColumn<SelectionTableRowData, PdfDescriptorLoadingStatus>, TableCell<SelectionTableRowData, PdfDescriptorLoadingStatus>> cellFactory() {
        return new Callback<TableColumn<SelectionTableRowData, PdfDescriptorLoadingStatus>, TableCell<SelectionTableRowData, PdfDescriptorLoadingStatus>>() {
            public TableCell<SelectionTableRowData, PdfDescriptorLoadingStatus> call(
                    TableColumn<SelectionTableRowData, PdfDescriptorLoadingStatus> param) {
                return new LoadingStatusTableCell();
            }
        };
    }

    public TableColumn<SelectionTableRowData, PdfDescriptorLoadingStatus> getTableColumn() {
        TableColumn<SelectionTableRowData, PdfDescriptorLoadingStatus> tableColumn = new TableColumn<>(getColumnTitle());
        tableColumn.setCellFactory(cellFactory());
        tableColumn.setCellValueFactory(cellValueFactory());
        tableColumn.setSortable(false);
        tableColumn.setMaxWidth(22);
        tableColumn.setMinWidth(22);
        return tableColumn;
    }

    public Comparator<PdfDescriptorLoadingStatus> comparator() {
        // not used
        return null;
    }

    private class LoadingStatusTableCell extends TableCell<SelectionTableRowData, PdfDescriptorLoadingStatus> implements
            PdfDocumentDescriptorProvider {
        private LoadingStatusIndicator loadingStatus = null;

        private LoadingStatusTableCell() {
            setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
            setText("");
            loadingStatus = new LoadingStatusIndicator(this, LoadingStatusColumn.this.getOwnerModule());
            setGraphic(loadingStatus);

        }

        public PdfDocumentDescriptor getPdfDocumentDescriptor() {
            return (SelectionTableRowData) getTableRow().getItem();
        }

        @Override
        public void updateItem(final PdfDescriptorLoadingStatus item, boolean empty) {
            super.updateItem(item, empty);
            if (item != null) {
                loadingStatus.setLoadingStatus(item);
            }
        }
    }
}
