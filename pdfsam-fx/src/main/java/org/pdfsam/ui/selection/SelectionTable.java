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
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Arrays;
import java.util.Collections;

import javafx.collections.ObservableList;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableView;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.module.ModuleOwned;
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
        // TODO single interval
        getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
        Arrays.stream(columns).forEach(c -> getColumns().add(c.getTableColumn()));
        setColumnResizePolicy(CONSTRAINED_RESIZE_POLICY);
        setTableMenuButtonVisible(true);
        // TODO
        // setPlaceholder(dropImage);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventStation
    public String getOwnerModule() {
        return ownerModule;
    }

    @EventListener
    public void onLoadDocumentsCompletion(final PdfLoadCompletedEvent event) {
        event.getDocuments().forEach(d -> getItems().add(new SelectionTableRowData(d)));
    }

    @EventListener
    public void onClear(final ClearSelectionTableEvent event) {
        getItems().clear();
    }

    @EventListener
    public void onRemoveSelected(RemoveSelectedEvent event) {
        getItems().removeAll(getSelectionModel().getSelectedItems());
    }

    @EventListener
    public void onMoveSelected(final MoveSelectedEvent event) {
        getSortOrder().clear();
        ObservableList<Integer> selected = getSelectionModel().getSelectedIndices();
        if (event.getType() == MoveType.DOWN) {
            moveDownIndexes(selected);
            // setRowSelectionInterval(selected[0] + 1, selected[selected.length - 1] + 1);
        } else {
            moveUpIndexes(selected);
            // setRowSelectionInterval(selected[0] - 1, selected[selected.length - 1] - 1);
        }
    }

    public void moveUpIndexes(ObservableList<Integer> toMove) {
        if (!toMove.isEmpty() && toMove.size() < getItems().size() && toMove.get(0) > 0) {
            Collections.rotate(getItems().subList(toMove.get(0) - 1, toMove.get(toMove.size() - 1) + 1), -1);
        }
    }

    public void moveDownIndexes(ObservableList<Integer> toMove) {
        if (!toMove.isEmpty() && toMove.size() < getItems().size()
                && toMove.get(toMove.size() - 1) < getItems().size() - 1) {
            Collections.rotate(getItems().subList(toMove.get(0), toMove.get(toMove.size() - 1) + 2), 1);
        }
    }
}
