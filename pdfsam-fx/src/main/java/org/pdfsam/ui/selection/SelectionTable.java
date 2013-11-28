/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 27/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.selection;

import static org.apache.commons.lang3.StringUtils.defaultString;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableView;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadCompletedEvent;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;

/**
 * Table displaying selected pdf documents
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTable extends TableView<SelectionTableRowData> implements ModuleOwned {

    private String ownerModule = StringUtils.EMPTY;

    public SelectionTable(String ownerModule, SelectionTableColumn<?>... columns) {
        this.ownerModule = defaultString(ownerModule);
        for (SelectionTableColumn<?> column : columns) {
            getColumns().add(column.getTableColumn());
        }
        getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
        /*
         * Arrays.stream(columns).forEachOrdered(column ->{ TableColumn tableColumn = new TableColumn(column.getColumnTitle()); });
         */
    }

    @EventStation
    public String getOwnerModule() {
        return ownerModule;
    }

    @EventListener
    public void onLoadDocumentsCompletion(final PdfLoadCompletedEvent event) {
        for (PdfDocumentDescriptor document : event.getDocuments()) {
            getItems().add(new SelectionTableRowData(document));
        }
        // getItems().addAll(event.getDocuments().parallelStream().map(SelectionTableRowData::new).collect(Collectors.toList()));
    }
}
