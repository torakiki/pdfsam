/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.ENCRYPTED;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.WITH_ERRORS;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Comparator;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.pdfsam.ui.selection.LoadingStatusIndicatorUpdater;
import org.pdfsam.ui.selection.ShowPasswordFieldPopupRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.beans.value.ObservableValue;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.input.MouseEvent;
import javafx.util.Callback;

/**
 * Definition of the {@link PdfDescriptorLoadingStatus} column of the selection table
 * 
 * @author Andrea Vacondio
 *
 */
public class LoadingColumn implements SelectionTableColumn<PdfDescriptorLoadingStatus>, ModuleOwned {
    private static final Logger LOG = LoggerFactory.getLogger(LoadingColumn.class);
    private String ownerModule = StringUtils.EMPTY;

    public LoadingColumn(String ownerModule) {
        this.ownerModule = defaultString(ownerModule);
    }

    @Override
    public String getOwnerModule() {
        return ownerModule;
    }

    @Override
    public String getColumnTitle() {
        return null;
    }

    @Override
    public ObservableValue<PdfDescriptorLoadingStatus> getObservableValue(SelectionTableRowData data) {
        return data.descriptor().loadingStatus();
    }

    @Override
    public String getTextValue(PdfDescriptorLoadingStatus item) {
        return (item != null && item.getIcon() != null) ? item.getIcon().toString() : "";
    }

    @Override
    public Callback<TableColumn<SelectionTableRowData, PdfDescriptorLoadingStatus>, TableCell<SelectionTableRowData, PdfDescriptorLoadingStatus>> cellFactory() {
        return new Callback<>() {
            @Override
            public TableCell<SelectionTableRowData, PdfDescriptorLoadingStatus> call(
                    TableColumn<SelectionTableRowData, PdfDescriptorLoadingStatus> param) {
                return new LoadingStatusCell();
            }
        };
    }

    @Override
    public Comparator<PdfDescriptorLoadingStatus> comparator() {
        return Comparator.naturalOrder();
    }

    @Override
    public TableColumn<SelectionTableRowData, PdfDescriptorLoadingStatus> getTableColumn() {
        TableColumn<SelectionTableRowData, PdfDescriptorLoadingStatus> tableColumn = new TableColumn<>(getColumnTitle());
        tableColumn.setCellFactory(cellFactory());
        tableColumn.setCellValueFactory(cellValueFactory());
        tableColumn.setComparator(null);
        tableColumn.setSortable(false);
        tableColumn.setMaxWidth(26);
        tableColumn.setMinWidth(26);
        return tableColumn;
    }

    /**
     * {@link TableCell} showing a loading status indicator. In some cases it reacts to the user click taking appropriate action.
     * 
     * @author Andrea Vacondio
     *
     */
    private class LoadingStatusCell extends TableCell<SelectionTableRowData, PdfDescriptorLoadingStatus> {

        private LoadingStatusIndicatorUpdater updater;

        LoadingStatusCell() {
            getStyleClass().addAll("encryption-status");
            this.addEventFilter(
                    MouseEvent.MOUSE_CLICKED,
                    e -> {
                        if (getItem() == ENCRYPTED) {
                            eventStudio().broadcast(
                                    new ShowPasswordFieldPopupRequest(getPdfDocumentDescriptor(), this),
                                    getOwnerModule());
                        } else if (getItem() == WITH_ERRORS) {
                            eventStudio().broadcast(ShowStageRequest.INSTANCE, "LogStage");
                        }
                    });
            updater = new LoadingStatusIndicatorUpdater(this);
        }

        @Override
        public void updateItem(final PdfDescriptorLoadingStatus item, boolean empty) {
            super.updateItem(item, empty);
            LOG.trace("Updating idicator for new status {} for row {}", item, getTableRow().getIndex());
            updater.accept(item);
        }

        public PdfDocumentDescriptor getPdfDocumentDescriptor() {
            return ((SelectionTableRowData) getTableRow().getItem()).descriptor();
        }
    }
}
