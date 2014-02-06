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
import static org.pdfsam.pdf.PdfDocumentDescriptor.newDescriptorNoPassword;
import static org.pdfsam.ui.selection.SelectionChangedEvent.clearSelectionEvent;
import static org.pdfsam.ui.selection.SelectionChangedEvent.select;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Stream;

import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableView;
import javafx.scene.input.DragEvent;
import javafx.scene.input.TransferMode;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadCompletedEvent;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.event.SetDestinationEvent;
import org.pdfsam.ui.selection.MoveType.SelectionAndFocus;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Table displaying selected pdf documents
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTable extends TableView<SelectionTableRowData> implements ModuleOwned {
    private static final Logger LOG = LoggerFactory.getLogger(SelectionTable.class);
    private String ownerModule = StringUtils.EMPTY;
    private Label placeHolder = new Label(DefaultI18nContext.getInstance().i18n("Drag and drop PDF files here"));

    public SelectionTable(String ownerModule, SelectionTableColumn<?>... columns) {
        this.ownerModule = defaultString(ownerModule);
        setEditable(true);
        getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
        Arrays.stream(columns).forEach(c -> getColumns().add(c.getTableColumn()));
        setColumnResizePolicy(CONSTRAINED_RESIZE_POLICY);
        setTableMenuButtonVisible(true);
        getSelectionModel().getSelectedIndices().addListener((Change<? extends Integer> c) -> {

            ObservableList<? extends Integer> selected = c.getList();
            if (selected.isEmpty()) {
                eventStudio().broadcast(clearSelectionEvent(), ownerModule);
                LOG.trace("Selection cleared for {}", ownerModule);
            } else {
                SelectionChangedEvent newSelectionEvent = select(selected).ofTotalRows(getItems().size());
                eventStudio().broadcast(newSelectionEvent, ownerModule);
                LOG.trace("{} for {}", newSelectionEvent, ownerModule);
            }

        });
        placeHolder.getStyleClass().addAll(Style.DROP_PLACEHOLDER.css());
        placeHolder.setDisable(true);
        setPlaceholder(placeHolder);
        setOnDragOver(e -> dragConsume(e, this.onDragOverConsumer()));
        setOnDragEntered(e -> dragConsume(e, this.onDragEnteredConsumer()));
        setOnDragExited(this::onDragExited);
        setOnDragDropped(e -> dragConsume(e, this.onDragDropped()));
        initContextMenu();
        eventStudio().addAnnotatedListeners(this);
    }

    private void initContextMenu() {
        MenuItem infoItem = new MenuItem(DefaultI18nContext.getInstance().i18n("Document properties"));
        // infoItem.setOnAction(e -> eventStudio().broadcast(showDocumentProperties, getOwnerModule()));
        infoItem.getStyleClass().add("ctx-menu-item");
        MenuItem setDestinationItem = new MenuItem(DefaultI18nContext.getInstance().i18n("Set output"));
        setDestinationItem.setOnAction(e -> {
            File outFile = new File(
                    getSelectionModel().getSelectedItem().getDocumentDescriptor().getFile().getParent(), "out.pdf");
            eventStudio().broadcast(new SetDestinationEvent(outFile), getOwnerModule());
        });
        setDestinationItem.getStyleClass().add("ctx-menu-item");

        getSelectionModel().getSelectedIndices().addListener((Observable o) -> {
            boolean singleSelection = getSelectionModel().getSelectedIndices().size() != 1;
            setDestinationItem.setDisable(singleSelection);
            infoItem.setDisable(singleSelection);
        });

        setContextMenu(new ContextMenu(infoItem, setDestinationItem));
    }

    private void dragConsume(DragEvent e, Consumer<DragEvent> c) {
        List<File> files = e.getDragboard().getFiles();
        if (files != null && !files.isEmpty()) {
            c.accept(e);
        }
        e.consume();
    }

    private Consumer<DragEvent> onDragOverConsumer() {
        return (DragEvent e) -> {
            e.acceptTransferModes(TransferMode.COPY_OR_MOVE);
        };
    }

    private Consumer<DragEvent> onDragEnteredConsumer() {
        return (DragEvent e) -> {
            placeHolder.setDisable(false);
        };
    }

    private void onDragExited(DragEvent e) {
        placeHolder.setDisable(true);
        e.consume();
    }

    private Consumer<DragEvent> onDragDropped() {
        return (DragEvent e) -> {
            final PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(getOwnerModule());
            Stream<PdfDocumentDescriptor> descriptors = e.getDragboard().getFiles().parallelStream()
                    .filter(f -> FileType.PDF.matches(f.getName())).map(PdfDocumentDescriptor::newDescriptorNoPassword);
            descriptors.forEach(loadEvent::add);
            eventStudio().broadcast(loadEvent, getOwnerModule());
            eventStudio().broadcast(loadEvent);
            e.setDropCompleted(true);
        };
    }

    @EventStation
    public String getOwnerModule() {
        return ownerModule;
    }

    @EventListener
    public void onLoadDocumentsCompletion(final PdfLoadCompletedEvent event) {
        Platform.runLater(() -> event.getDocuments().forEach(d -> getItems().add(new SelectionTableRowData(d))));
    }

    @EventListener
    public void onClear(final ClearSelectionTableEvent event) {
        getSelectionModel().clearSelection();
        getItems().clear();
    }

    @EventListener
    public void onRemoveSelected(RemoveSelectedEvent event) {
        getItems().removeAll(new ArrayList<>(getSelectionModel().getSelectedItems()));
        getSelectionModel().clearSelection();
    }

    @EventListener
    public void onMoveSelected(final MoveSelectedEvent event) {
        getSortOrder().clear();
        ObservableList<Integer> selectedIndices = getSelectionModel().getSelectedIndices();
        Integer[] selected = selectedIndices.toArray(new Integer[selectedIndices.size()]);
        int focus = getFocusModel().getFocusedIndex();
        getSelectionModel().clearSelection();
        SelectionAndFocus newSelection = event.getType().move(selected, getItems(), focus);
        if (!SelectionAndFocus.NULL.equals(newSelection)) {
            LOG.trace("Changing selection to {}", newSelection);
            getSelectionModel().selectIndices(newSelection.getRow(), newSelection.getRows());
            getFocusModel().focus(newSelection.getFocus());
        }
    }
}
