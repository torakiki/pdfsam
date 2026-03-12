/*
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
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
import javafx.scene.input.MouseEvent;
import javafx.util.Callback;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.tool.BaseToolBound;
import org.pdfsam.model.ui.ShowLogMessagesRequest;
import org.pdfsam.ui.components.selection.LoadingStatusIndicatorUpdater;
import org.pdfsam.ui.components.selection.ShowPasswordFieldPopupRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Comparator;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.model.pdf.PdfDescriptorLoadingStatus.ENCRYPTED;
import static org.pdfsam.model.pdf.PdfDescriptorLoadingStatus.WITH_ERRORS;

/**
 * Definition of the {@link PdfDescriptorLoadingStatus} column of the selection table
 *
 * @author Andrea Vacondio
 */
public class LoadingColumn extends BaseToolBound implements SelectionTableColumn<PdfDescriptorLoadingStatus> {
    private static final Logger LOG = LoggerFactory.getLogger(LoadingColumn.class);

    public LoadingColumn(String ownerModule) {
        super(defaultString(ownerModule));
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
        return param -> new LoadingStatusCell();
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

        private final LoadingStatusIndicatorUpdater updater;

        LoadingStatusCell() {
            getStyleClass().addAll("encryption-status");
            this.addEventFilter(
                    MouseEvent.MOUSE_CLICKED,
                    e -> {
                        if (getItem() == ENCRYPTED) {
                            eventStudio().broadcast(new ShowPasswordFieldPopupRequest(this, getPdfDocumentDescriptor()),
                                    toolBinding());
                        } else if (getItem() == WITH_ERRORS) {
                            eventStudio().broadcast(new ShowLogMessagesRequest());
                        }
                    });
            updater = new LoadingStatusIndicatorUpdater(this);
        }

        @Override
        public void updateItem(final PdfDescriptorLoadingStatus item, boolean empty) {
            super.updateItem(item, empty);
            LOG.trace("Updating indicator for new status {} for row {}", item, getTableRow().getIndex());
            updater.accept(item);
        }

        public PdfDocumentDescriptor getPdfDocumentDescriptor() {
            return getTableRow().getItem().descriptor();
        }
    }
}
