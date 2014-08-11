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
package org.pdfsam.ui.selection.multiple;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.ui.commons.SetDestinationRequest.requestDestination;
import static org.pdfsam.ui.commons.SetDestinationRequest.requestFallbackDestination;
import static org.pdfsam.ui.selection.multiple.SelectionChangedEvent.clearSelectionEvent;
import static org.pdfsam.ui.selection.multiple.SelectionChangedEvent.select;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

import javafx.application.Platform;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TableView;
import javafx.scene.input.DragEvent;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.TransferMode;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.commons.OpenFileRequest;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.pdfsam.ui.selection.multiple.move.MoveSelectedEvent;
import org.pdfsam.ui.selection.multiple.move.MoveType;
import org.pdfsam.ui.selection.multiple.move.SelectionAndFocus;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;

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
        placeHolder.getStyleClass().add("drag-drop-placeholder");
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
        MenuItem infoItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Document properties"),
                AwesomeIcon.INFO);
        infoItem.setOnAction(e -> Platform.runLater(() -> {
            eventStudio().broadcast(new ShowPdfDescriptorRequest(getSelectionModel().getSelectedItem()));
        }));

        MenuItem setDestinationItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Set destination"),
                AwesomeIcon.FILE_PDF_ALT);
        setDestinationItem.setOnAction(e -> {
            eventStudio().broadcast(
                    requestDestination(getSelectionModel().getSelectedItem().getFile(), getOwnerModule()),
                    getOwnerModule());
        });

        MenuItem removeSelected = createMenuItem(DefaultI18nContext.getInstance().i18n("Remove"),
                AwesomeIcon.MINUS_SQUARE_ALT);
        removeSelected.setOnAction(e -> eventStudio().broadcast(new RemoveSelectedEvent(), getOwnerModule()));

        MenuItem moveTopSelected = createMenuItem(DefaultI18nContext.getInstance().i18n("Move to Top"),
                AwesomeIcon.ANGLE_DOUBLE_UP);
        moveTopSelected
                .setOnAction(e -> eventStudio().broadcast(new MoveSelectedEvent(MoveType.TOP), getOwnerModule()));

        MenuItem moveUpSelected = createMenuItem(DefaultI18nContext.getInstance().i18n("Move Up"), AwesomeIcon.ANGLE_UP);
        moveUpSelected.setOnAction(e -> eventStudio().broadcast(new MoveSelectedEvent(MoveType.UP), getOwnerModule()));

        MenuItem moveDownSelected = createMenuItem(DefaultI18nContext.getInstance().i18n("Move Down"),
                AwesomeIcon.ANGLE_DOWN);
        moveDownSelected.setOnAction(e -> eventStudio().broadcast(new MoveSelectedEvent(MoveType.DOWN),
                getOwnerModule()));

        MenuItem moveBottomSelected = createMenuItem(DefaultI18nContext.getInstance().i18n("Move to Bottom"),
                AwesomeIcon.ANGLE_DOUBLE_DOWN);
        moveBottomSelected.setOnAction(e -> eventStudio().broadcast(new MoveSelectedEvent(MoveType.BOTTOM),
                getOwnerModule()));

        MenuItem openFileItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Open"), AwesomeIcon.FILE_ALT);
        openFileItem.setOnAction(e -> {
            eventStudio().broadcast(new OpenFileRequest(getSelectionModel().getSelectedItem().getFile()));
        });

        MenuItem openFolderItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Open Folder"),
                AwesomeIcon.FOLDER_OPEN);
        openFolderItem.setOnAction(e -> {
            eventStudio().broadcast(
                    new OpenFileRequest(getSelectionModel().getSelectedItem().getFile().getParentFile()));
        });

        setDestinationItem.setAccelerator(new KeyCodeCombination(KeyCode.O, KeyCombination.ALT_DOWN));
        removeSelected.setAccelerator(new KeyCodeCombination(KeyCode.CANCEL));
        moveBottomSelected.setAccelerator(new KeyCodeCombination(KeyCode.END, KeyCombination.ALT_DOWN));
        moveDownSelected.setAccelerator(new KeyCodeCombination(KeyCode.DOWN, KeyCombination.ALT_DOWN));
        moveUpSelected.setAccelerator(new KeyCodeCombination(KeyCode.UP, KeyCombination.ALT_DOWN));
        moveTopSelected.setAccelerator(new KeyCodeCombination(KeyCode.HOME, KeyCombination.ALT_DOWN));

        eventStudio().add(SelectionChangedEvent.class, (SelectionChangedEvent e) -> {
            setDestinationItem.setDisable(!e.isSingleSelection());
            infoItem.setDisable(!e.isSingleSelection());
            openFileItem.setDisable(!e.isSingleSelection());
            openFolderItem.setDisable(!e.isSingleSelection());
            removeSelected.setDisable(e.isClearSelection());
            moveTopSelected.setDisable(!e.canMove(MoveType.TOP));
            moveUpSelected.setDisable(!e.canMove(MoveType.UP));
            moveDownSelected.setDisable(!e.canMove(MoveType.DOWN));
            moveBottomSelected.setDisable(!e.canMove(MoveType.BOTTOM));

        }, getOwnerModule());
        setContextMenu(new ContextMenu(setDestinationItem, new SeparatorMenuItem(), removeSelected, moveTopSelected,
                moveUpSelected, moveDownSelected, moveBottomSelected, new SeparatorMenuItem(), infoItem, openFileItem,
                openFolderItem));
    }

    private MenuItem createMenuItem(String text, AwesomeIcon icon) {
        MenuItem item = new MenuItem(text);
        AwesomeDude.setIcon(item, icon);
        item.setDisable(true);
        return item;
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
            final PdfLoadRequestEvent<SelectionTableRowData> loadEvent = new PdfLoadRequestEvent<>(getOwnerModule());
            e.getDragboard().getFiles().stream().filter(f -> FileType.PDF.matches(f.getName()))
                    .map(SelectionTableRowData::new).forEach(loadEvent::add);
            eventStudio().broadcast(loadEvent, getOwnerModule());
            e.setDropCompleted(true);
        };
    }

    @EventStation
    public String getOwnerModule() {
        return ownerModule;
    }

    @EventListener(priority = Integer.MIN_VALUE)
    public void onLoadDocumentsRequest(PdfLoadRequestEvent<SelectionTableRowData> loadEvent) {
        getItems().addAll(loadEvent.getDocuments());
        loadEvent
                .getDocuments()
                .stream()
                .findFirst()
                .ifPresent(
                        f -> eventStudio().broadcast(requestFallbackDestination(f.getFile(), getOwnerModule()),
                                getOwnerModule()));
        eventStudio().broadcast(loadEvent);
    }

    @EventListener
    public void onClear(final ClearSelectionTableEvent event) {
        getItems().forEach(d -> d.invalidate());
        getSelectionModel().clearSelection();
        getItems().clear();
    }

    @EventListener
    public void onRemoveSelected(RemoveSelectedEvent event) {
        getSelectionModel().getSelectedItems().forEach(d -> {
            d.invalidate();
            getItems().remove(d);
        });
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
            scrollTo(newSelection.getFocus());
        }
    }
}
