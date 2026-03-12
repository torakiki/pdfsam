/*
 * This file is part of the PDF Split And Merge source code
 * Created on 27/nov/2013
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

import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.css.PseudoClass;
import javafx.geometry.Orientation;
import javafx.geometry.Point2D;
import javafx.scene.Scene;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ScrollBar;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.DataFormat;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.Region;
import javafx.stage.Window;
import javafx.util.Duration;
import org.kordamp.ikonli.Ikon;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.core.support.EncryptionUtils;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.io.NativeOpenFileRequest;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.model.ui.ShowPdfDescriptorRequest;
import org.pdfsam.model.ui.ShowStageRequest;
import org.pdfsam.model.ui.dnd.FilesDroppedEvent;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.selection.PasswordFieldPopup;
import org.pdfsam.ui.components.selection.RemoveSelectedEvent;
import org.pdfsam.ui.components.selection.SetPaceRequest;
import org.pdfsam.ui.components.selection.SetPageRangesRequest;
import org.pdfsam.ui.components.selection.ShowPasswordFieldPopupRequest;
import org.pdfsam.ui.components.selection.multiple.move.MoveSelectedRequest;
import org.pdfsam.ui.components.selection.multiple.move.MoveType;
import org.pdfsam.ui.components.selection.multiple.move.SelectionAndFocus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static java.util.Objects.nonNull;
import static java.util.Optional.of;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.core.support.EncryptionUtils.encrypt;
import static org.pdfsam.core.support.io.ObjectCollectionWriter.writeContent;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.model.ui.SetDestinationRequest.requestDestination;
import static org.pdfsam.model.ui.SetDestinationRequest.requestFallbackDestination;

/**
 * Table displaying selected pdf documents
 *
 * @author Andrea Vacondio
 */
public class SelectionTable extends TableView<SelectionTableRowData> implements ToolBound, RestorableView {

    private static final Logger LOG = LoggerFactory.getLogger(SelectionTable.class);
    private static final PseudoClass DRAG_HOVERED_TOP_ROW_PSEUDO_CLASS = PseudoClass.getPseudoClass(
            "drag-hovered-row-top");
    private static final PseudoClass DRAG_HOVERED_BOTTOM_ROW_PSEUDO_CLASS = PseudoClass.getPseudoClass(
            "drag-hovered-row-bottom");
    private static final DataFormat DND_TABLE_SELECTION_MIME_TYPE = new DataFormat(
            "application/x-java-table-selection-list");

    private final String toolBinding;
    private final Label placeHolder = new Label(i18n().tr("Drag and drop PDF files here or use the Add button above"));
    private final PasswordFieldPopup passwordPopup;
    private final IntegerProperty hoverIndex = new SimpleIntegerProperty(-1);
    private Consumer<SelectionChangedEvent> selectionChangedConsumer;

    private final Timeline scrollTimeline = new Timeline();
    private double scrollDirection = 0;

    public SelectionTable(String toolBinding, boolean canDuplicateItems, boolean canMove,
            TableColumnProvider<?>... columns) {
        this.toolBinding = defaultString(toolBinding);
        setEditable(true);
        getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
        getColumns().add(new IndexColumn());
        Arrays.stream(columns).forEach(c -> getColumns().add(c.getTableColumn()));
        setColumnResizePolicy(CONSTRAINED_RESIZE_POLICY_FLEX_LAST_COLUMN);
        getStyleClass().add("selection-table");
        initDragAndDrop(canMove);
        getSelectionModel().getSelectedIndices().addListener((Change<? extends Integer> c) -> {

            ObservableList<? extends Integer> selected = c.getList();
            if (selected.isEmpty()) {
                eventStudio().broadcast(SelectionChangedEvent.clearSelectionEvent(), toolBinding);
                LOG.trace("Selection cleared for {}", toolBinding);
            } else {
                SelectionChangedEvent newSelectionEvent = SelectionChangedEvent.select(selected)
                        .ofTotalRows(getItems().size());
                eventStudio().broadcast(newSelectionEvent, toolBinding);
                LOG.trace("{} for {}", newSelectionEvent, toolBinding);
            }

        });
        placeHolder.getStyleClass().add("drag-drop-placeholder");
        placeHolder.setDisable(true);
        setPlaceholder(placeHolder);
        setAccessibleText(i18n().tr("PDF documents selection table"));
        setAccessibleRoleDescription(i18n().tr("PDF selection table"));
        getItems().addListener((Change<? extends SelectionTableRowData> c) -> setAccessibleText(
                i18n().tr("PDF documents selection table, {0} documents", Integer.toString(getItems().size()))));
        passwordPopup = new PasswordFieldPopup(this.toolBinding);

        ContextMenu contextMenu = new ContextMenu();
        initTopSectionContextMenu(contextMenu, Arrays.stream(columns).anyMatch(PageRangesColumn.class::isInstance),
                Arrays.stream(columns).anyMatch(PaceColumn.class::isInstance));
        initItemsSectionContextMenu(contextMenu, canDuplicateItems, canMove);
        initBottomSectionContextMenu(contextMenu);
        setContextMenu(contextMenu);
        eventStudio().addAnnotatedListeners(this);
        eventStudio().add(SelectionChangedEvent.class, e -> selectionChangedConsumer.accept(e), toolBinding);
    }

    private void initTopSectionContextMenu(ContextMenu contextMenu, boolean hasRanges, boolean hasPace) {
        MenuItem setDestinationItem = createMenuItem(i18n().tr("Set destination"), UniconsLine.CROSSHAIR);
        setDestinationItem.setOnAction(e -> eventStudio().broadcast(
                requestDestination(getSelectionModel().getSelectedItem().descriptor().getFile(), toolBinding()),
                toolBinding()));
        setDestinationItem.setAccelerator(new KeyCodeCombination(KeyCode.O, KeyCombination.ALT_DOWN));

        selectionChangedConsumer = e -> setDestinationItem.setDisable(!e.isSingleSelection());
        contextMenu.getItems().add(setDestinationItem);

        if (hasRanges) {
            MenuItem setPageRangesItem = createMenuItem(i18n().tr("Set as range for all"),
                    UniconsLine.RIGHT_INDENT_ALT);
            setPageRangesItem.setOnAction(e -> eventStudio().broadcast(
                    new SetPageRangesRequest(getSelectionModel().getSelectedItem().pageSelection.get()),
                    toolBinding()));
            setPageRangesItem.setAccelerator(new KeyCodeCombination(KeyCode.R, KeyCombination.SHORTCUT_DOWN));
            selectionChangedConsumer = selectionChangedConsumer.andThen(
                    e -> setPageRangesItem.setDisable(!e.isSingleSelection()));
            contextMenu.getItems().add(setPageRangesItem);
        }
        if (hasPace) {
            MenuItem setAsPaceItem = createMenuItem(i18n().tr("Set as pace for all"), UniconsLine.ELLIPSIS_V);
            setAsPaceItem.setOnAction(
                    e -> eventStudio().broadcast(new SetPaceRequest(getSelectionModel().getSelectedItem().pace.get()),
                            toolBinding()));
            setAsPaceItem.setAccelerator(new KeyCodeCombination(KeyCode.P, KeyCombination.SHORTCUT_DOWN));
            selectionChangedConsumer = selectionChangedConsumer.andThen(
                    e -> setAsPaceItem.setDisable(!e.isSingleSelection()));
            contextMenu.getItems().add(setAsPaceItem);
        }
        contextMenu.getItems().add(new SeparatorMenuItem());
    }

    private void initItemsSectionContextMenu(ContextMenu contextMenu, boolean canDuplicate, boolean canMove) {

        MenuItem removeSelected = createMenuItem(i18n().tr("Remove"), UniconsLine.MINUS);
        removeSelected.setOnAction(e -> eventStudio().broadcast(new RemoveSelectedEvent(), toolBinding()));
        removeSelected.setAccelerator(new KeyCodeCombination(KeyCode.DELETE));
        contextMenu.getItems().add(removeSelected);
        selectionChangedConsumer = selectionChangedConsumer.andThen(
                e -> removeSelected.setDisable(e.isClearSelection()));
        if (canMove) {
            MenuItem moveTopSelected = createMenuItem(i18n().tr("Move to Top"), UniconsLine.ANGLE_DOUBLE_UP);
            moveTopSelected.setOnAction(
                    e -> eventStudio().broadcast(new MoveSelectedRequest(MoveType.TOP), toolBinding()));

            MenuItem moveUpSelected = createMenuItem(i18n().tr("Move Up"), UniconsLine.ANGLE_UP);
            moveUpSelected.setOnAction(
                    e -> eventStudio().broadcast(new MoveSelectedRequest(MoveType.UP), toolBinding()));

            MenuItem moveDownSelected = createMenuItem(i18n().tr("Move Down"), UniconsLine.ANGLE_DOWN);
            moveDownSelected.setOnAction(
                    e -> eventStudio().broadcast(new MoveSelectedRequest(MoveType.DOWN), toolBinding()));

            MenuItem moveBottomSelected = createMenuItem(i18n().tr("Move to Bottom"), UniconsLine.ANGLE_DOUBLE_DOWN);
            moveBottomSelected.setOnAction(
                    e -> eventStudio().broadcast(new MoveSelectedRequest(MoveType.BOTTOM), toolBinding()));

            contextMenu.getItems().addAll(moveTopSelected, moveUpSelected, moveDownSelected, moveBottomSelected);

            moveBottomSelected.setAccelerator(new KeyCodeCombination(KeyCode.END, KeyCombination.ALT_DOWN));
            moveDownSelected.setAccelerator(new KeyCodeCombination(KeyCode.DOWN, KeyCombination.ALT_DOWN));
            moveUpSelected.setAccelerator(new KeyCodeCombination(KeyCode.UP, KeyCombination.ALT_DOWN));
            moveTopSelected.setAccelerator(new KeyCodeCombination(KeyCode.HOME, KeyCombination.ALT_DOWN));

            MenuItem unlockSelected = createMenuItem(i18n().tr("Unlock selected"), UniconsLine.UNLOCK_ALT);
            unlockSelected.setOnAction(e -> {
                TableViewSelectionModel<SelectionTableRowData> sm = getSelectionModel();
                PdfDocumentDescriptor[] descriptors = sm.getSelectedItems().stream()
                        .map(SelectionTableRowData::descriptor).toArray(PdfDocumentDescriptor[]::new);
                eventStudio().broadcast(new ShowPasswordFieldPopupRequest(this, descriptors), toolBinding());
            });
            unlockSelected.setAccelerator(new KeyCodeCombination(KeyCode.U, KeyCombination.CONTROL_DOWN));
            unlockSelected.disableProperty().bind(Bindings.createBooleanBinding(() -> {
                TableViewSelectionModel<SelectionTableRowData> sm = getSelectionModel();
                return sm.getSelectedItems().stream().noneMatch(
                        data -> data.descriptor().loadingStatus().getValue() == PdfDescriptorLoadingStatus.ENCRYPTED);
            }, getSelectionModel().getSelectedItems()));
            contextMenu.getItems().add(unlockSelected);

            selectionChangedConsumer = selectionChangedConsumer.andThen(e -> {
                moveTopSelected.setDisable(!e.canMove(MoveType.TOP));
                moveUpSelected.setDisable(!e.canMove(MoveType.UP));
                moveDownSelected.setDisable(!e.canMove(MoveType.DOWN));
                moveBottomSelected.setDisable(!e.canMove(MoveType.BOTTOM));
            });
        }
        if (canDuplicate) {
            Menu duplicateItem = new Menu(i18n().tr("Duplicate"));
            duplicateItem.setGraphic(FontIcon.of(UniconsLine.COPY));
            duplicateItem.setDisable(true);

            MenuItem duplicateTop = createMenuItem(i18n().tr("Duplicate at Top"), null);
            duplicateTop.setOnAction(
                    e -> eventStudio().broadcast(new DuplicateSelectedEvent(DuplicateType.TOP), toolBinding()));
            duplicateTop.setAccelerator(new KeyCodeCombination(KeyCode.DIGIT1, KeyCombination.ALT_DOWN));

            MenuItem duplicateUp = createMenuItem(i18n().tr("Duplicate Up"), null);
            duplicateUp.setOnAction(
                    e -> eventStudio().broadcast(new DuplicateSelectedEvent(DuplicateType.UP), toolBinding()));
            duplicateUp.setAccelerator(new KeyCodeCombination(KeyCode.DIGIT3, KeyCombination.ALT_DOWN));

            MenuItem duplicateDown = createMenuItem(i18n().tr("Duplicate Down"), null);
            duplicateDown.setOnAction(
                    e -> eventStudio().broadcast(new DuplicateSelectedEvent(DuplicateType.DOWN), toolBinding()));
            duplicateDown.setAccelerator(new KeyCodeCombination(KeyCode.DIGIT4, KeyCombination.ALT_DOWN));

            MenuItem duplicateBottom = createMenuItem(i18n().tr("Duplicate at Bottom"), null);
            duplicateBottom.setOnAction(
                    e -> eventStudio().broadcast(new DuplicateSelectedEvent(DuplicateType.BOTTOM), toolBinding()));
            duplicateBottom.setAccelerator(new KeyCodeCombination(KeyCode.DIGIT2, KeyCombination.ALT_DOWN));

            duplicateItem.getItems().addAll(duplicateTop, duplicateUp, duplicateDown, duplicateBottom);
            contextMenu.getItems().add(duplicateItem);

            selectionChangedConsumer = selectionChangedConsumer.andThen(e -> {
                duplicateItem.setDisable(e.isClearSelection());
                duplicateTop.setDisable(e.isClearSelection());
                duplicateBottom.setDisable(e.isClearSelection());
                duplicateUp.setDisable(e.isClearSelection());
                duplicateDown.setDisable(e.isClearSelection());
            });
        }
    }

    private void initBottomSectionContextMenu(ContextMenu contextMenu) {

        MenuItem copyItem = createMenuItem(i18n().tr("Copy to clipboard"), UniconsLine.COPY_ALT);
        copyItem.setOnAction(e -> copySelectedToClipboard());

        MenuItem infoItem = createMenuItem(i18n().tr("Document properties"), UniconsLine.INFO_CIRCLE);
        infoItem.setOnAction(e -> Platform.runLater(() -> {
            eventStudio().broadcast(ShowStageRequest.INSTANCE, "InfoStage");
            eventStudio().broadcast(new ShowPdfDescriptorRequest(getSelectionModel().getSelectedItem().descriptor()));
        }));

        MenuItem openFileItem = createMenuItem(i18n().tr("Open"), UniconsLine.FILE_ALT);
        openFileItem.setOnAction(e -> eventStudio().broadcast(
                new NativeOpenFileRequest(getSelectionModel().getSelectedItem().descriptor().getFile())));

        MenuItem openFolderItem = createMenuItem(i18n().tr("Open Folder"), UniconsLine.FOLDER);
        openFolderItem.setOnAction(e -> eventStudio().broadcast(new NativeOpenFileRequest(
                getSelectionModel().getSelectedItem().descriptor().getFile().getParentFile())));

        copyItem.setAccelerator(new KeyCodeCombination(KeyCode.C, KeyCombination.SHORTCUT_DOWN));
        infoItem.setAccelerator(new KeyCodeCombination(KeyCode.P, KeyCombination.ALT_DOWN));
        openFileItem.setAccelerator(new KeyCodeCombination(KeyCode.O, KeyCombination.SHORTCUT_DOWN));
        openFolderItem.setAccelerator(
                new KeyCodeCombination(KeyCode.O, KeyCombination.SHORTCUT_DOWN, KeyCombination.ALT_DOWN));

        contextMenu.getItems().addAll(new SeparatorMenuItem(), copyItem, infoItem, openFileItem, openFolderItem);

        selectionChangedConsumer = selectionChangedConsumer.andThen(e -> {
            copyItem.setDisable(e.isClearSelection());
            infoItem.setDisable(!e.isSingleSelection());
            openFileItem.setDisable(!e.isSingleSelection());
            openFolderItem.setDisable(!e.isSingleSelection());
        });
    }

    private MenuItem createMenuItem(String text, Ikon icon) {
        var item = new MenuItem(text);
        // TODO set font size 1.1 em
        if (nonNull(icon)) {
            item.setGraphic(FontIcon.of(icon));
        }
        item.setDisable(true);
        return item;
    }

    private void initDragAndDrop(boolean canMove) {
        scrollTimeline.setCycleCount(Timeline.INDEFINITE);
        scrollTimeline.getKeyFrames().add(new KeyFrame(Duration.millis(20), "Scroll", event -> dragScroll()));

        addEventFilter(DragEvent.DRAG_OVER, this::autoscrollIfNeeded);
        addEventFilter(DragEvent.DRAG_EXITED, this::stopAutoScrollIfNeeded);
        addEventFilter(DragEvent.DRAG_DROPPED, this::stopAutoScrollIfNeeded);
        addEventFilter(DragEvent.DRAG_DONE, this::stopAutoScrollIfNeeded);

        setOnDragOver(e -> dragConsume(e, this.onDragOverConsumer()));
        setOnDragEntered(e -> dragConsume(e, this.onDragEnteredConsumer()));
        setOnDragExited(this::onDragExited);
        setOnDragDropped(e -> dragConsume(e, this.onDragDropped()));
        if (canMove) {
            setRowFactory(tv -> {
                TableRow<SelectionTableRowData> row = new TableRow<>();
                row.setOnDragDetected(e -> {
                    ArrayList<Integer> selection = new ArrayList<>(getSelectionModel().getSelectedIndices());
                    if (!row.isEmpty() && !selection.isEmpty()) {
                        Dragboard db = row.startDragAndDrop(TransferMode.MOVE);
                        db.setDragView(row.snapshot(null, null));
                        ClipboardContent cc = new ClipboardContent();
                        cc.put(DND_TABLE_SELECTION_MIME_TYPE, selection);
                        db.setContent(cc);
                        e.consume();

                    }
                });

                row.setOnDragOver(e -> {
                    var rowIndex = row.getIndex();
                    var affectedRowIndex = (e.getY() > (row.getHeight() / 2)) ? (rowIndex + 1) : rowIndex;

                    hoverIndex.set(affectedRowIndex);

                    if (!row.isEmpty()) {
                        if (affectedRowIndex > rowIndex) {
                            activateHoverBottomPsuedoClass(row);
                        } else {
                            activateHoverTopPseudoClass(row);
                        }
                    } else {
                        clearHoverPseudoClasses(row);
                    }

                    if (e.getGestureSource() != row && e.getDragboard().hasContent(DND_TABLE_SELECTION_MIME_TYPE)) {
                        if (!((List<Integer>) e.getDragboard().getContent(DND_TABLE_SELECTION_MIME_TYPE)).contains(
                                rowIndex)) {
                            e.acceptTransferModes(TransferMode.MOVE);
                            e.consume();
                        }
                    }
                });
                row.setOnDragEntered(e -> {
                    if (!row.isEmpty() && e.getDragboard().hasContent(DND_TABLE_SELECTION_MIME_TYPE)) {
                        if (!((List<Integer>) e.getDragboard().getContent(DND_TABLE_SELECTION_MIME_TYPE)).contains(
                                row.getIndex())) {
                            row.setOpacity(0.6);
                        }
                    }
                });
                row.setOnDragExited(e -> {
                    clearHoverPseudoClasses(row);

                    if (!row.isEmpty() && e.getDragboard().hasContent(DND_TABLE_SELECTION_MIME_TYPE)) {
                        if (!((List<Integer>) e.getDragboard().getContent(DND_TABLE_SELECTION_MIME_TYPE)).contains(
                                row.getIndex())) {
                            row.setOpacity(1);
                        }
                    }
                });

                row.setOnDragDropped(e -> {
                    clearHoverPseudoClasses(row);

                    Dragboard db = e.getDragboard();
                    if (db.hasContent(DND_TABLE_SELECTION_MIME_TYPE)) {
                        Optional<SelectionTableRowData> focus = ofNullable(getFocusModel().getFocusedItem());
                        Optional<SelectionTableRowData> toDrop = of(row).filter(r -> !r.isEmpty())
                                .map(TableRow::getIndex).map(getItems()::get);

                        List<Integer> dragged = (List<Integer>) e.getDragboard()
                                .getContent(DND_TABLE_SELECTION_MIME_TYPE);
                        List<SelectionTableRowData> toMove = dragged.stream().map(getItems()::get)
                                .filter(Objects::nonNull).toList();
                        getItems().removeAll(toMove);

                        int dropIndex = getItems().size();
                        if (toDrop.isPresent()) {
                            int toDropNewIndex = toDrop.map(getItems()::indexOf).get();
                            if (toDropNewIndex == row.getIndex()) {
                                // we dropped up
                                dropIndex = toDropNewIndex;
                            } else {
                                // we dropped down
                                dropIndex = toDropNewIndex + 1;
                            }
                        }
                        getSortOrder().clear();
                        getItems().addAll(dropIndex, toMove);
                        e.setDropCompleted(true);
                        getSelectionModel().clearSelection();
                        getSelectionModel().selectRange(dropIndex, dropIndex + toMove.size());
                        focus.map(getItems()::indexOf).ifPresent(getFocusModel()::focus);
                        e.consume();
                    }
                });

                return row;
            });
        }
    }

    private static <T> void activateHoverTopPseudoClass(TableRow<T> row) {
        row.pseudoClassStateChanged(DRAG_HOVERED_TOP_ROW_PSEUDO_CLASS, true);
        row.pseudoClassStateChanged(DRAG_HOVERED_BOTTOM_ROW_PSEUDO_CLASS, false);
    }

    private static <T> void activateHoverBottomPsuedoClass(TableRow<T> row) {
        row.pseudoClassStateChanged(DRAG_HOVERED_TOP_ROW_PSEUDO_CLASS, false);
        row.pseudoClassStateChanged(DRAG_HOVERED_BOTTOM_ROW_PSEUDO_CLASS, true);
    }

    private static <T> void clearHoverPseudoClasses(TableRow<T> row) {
        row.pseudoClassStateChanged(DRAG_HOVERED_TOP_ROW_PSEUDO_CLASS, false);
        row.pseudoClassStateChanged(DRAG_HOVERED_BOTTOM_ROW_PSEUDO_CLASS, false);
    }

    private void dragConsume(DragEvent e, Consumer<DragEvent> c) {
        if (e.getDragboard().hasFiles()) {
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
        return (DragEvent e) -> placeHolder.setDisable(false);

    }

    private void onDragExited(DragEvent e) {
        placeHolder.setDisable(true);
        e.consume();
    }

    private Consumer<DragEvent> onDragDropped() {
        return (DragEvent e) -> {
            eventStudio().broadcast(new FilesDroppedEvent(toolBinding, true, e.getDragboard().getFiles()));
            e.setDropCompleted(true);
        };
    }

    @Override
    @EventStation
    public String toolBinding() {
        return toolBinding;
    }

    @EventListener(priority = Integer.MIN_VALUE)
    public void onLoadDocumentsRequest(PdfLoadRequest loadEvent) {
        var focus = ofNullable(getFocusModel().getFocusedItem());
        var toDrop = loadEvent.getDocuments().stream().map(SelectionTableRowData::new).toList();
        var hoverIndexValue = hoverIndex.get();
        var itemsCount = getItems().size();
        var dropIndex = (hoverIndexValue < 0 || hoverIndexValue > itemsCount) ? itemsCount : hoverIndexValue;

        getSortOrder().clear();
        getItems().addAll(dropIndex, toDrop);
        focus.map(getItems()::indexOf).ifPresent(getFocusModel()::focus);
        this.sort();

        loadEvent.getDocuments().stream().findFirst().ifPresent(
                f -> eventStudio().broadcast(requestFallbackDestination(f.getFile(), toolBinding()), toolBinding()));
        eventStudio().broadcast(loadEvent);
    }

    @EventListener
    public void onDuplicate(final DuplicateSelectedEvent event) {
        LOG.trace("Duplicating selected items");
        switch (event.type()) {
        case TOP -> getSelectionModel().getSelectedItems().stream().map(SelectionTableRowData::duplicate).toList()
                .reversed().forEach(getItems()::addFirst);
        case BOTTOM -> getSelectionModel().getSelectedItems().forEach(i -> getItems().addLast(i.duplicate()));
        case UP -> {
            var index = getSelectionModel().getSelectedIndices().stream().min(Integer::compareTo).get();
            getSelectionModel().getSelectedItems().stream().map(SelectionTableRowData::duplicate).toList().reversed()
                    .forEach(e -> getItems().add(index, e));
        }
        case DOWN -> {
            var index = getSelectionModel().getSelectedIndices().stream().max(Integer::compareTo).get() + 1;
            getSelectionModel().getSelectedItems().stream().map(SelectionTableRowData::duplicate).toList().reversed()
                    .forEach(e -> getItems().add(index, e));
        }
        }
        this.sort();
    }

    @EventListener
    public void onClear(final ClearToolRequest event) {
        getItems().forEach(d -> d.descriptor().releaseAll());
        getSelectionModel().clearSelection();
        getItems().clear();
    }

    @EventListener
    public void onRemoveSelected(RemoveSelectedEvent event) {
        SortedSet<Integer> indices = new TreeSet<>(Collections.reverseOrder());
        indices.addAll(getSelectionModel().getSelectedIndices());
        LOG.trace("Removing {} items", indices.size());
        indices.forEach(i -> getItems().remove(i.intValue()).invalidate());
        requestFocus();
    }

    @EventListener
    public void onMoveSelected(final MoveSelectedRequest event) {
        getSortOrder().clear();
        ObservableList<Integer> selectedIndices = getSelectionModel().getSelectedIndices();
        Integer[] selected = selectedIndices.toArray(new Integer[0]);
        int focus = getFocusModel().getFocusedIndex();
        getSelectionModel().clearSelection();
        SelectionAndFocus newSelection = event.type().move(selected, getItems(), focus);
        if (!SelectionAndFocus.NULL.equals(newSelection)) {
            LOG.trace("Changing selection to {}", newSelection);
            getSelectionModel().selectIndices(newSelection.row(), newSelection.getRows());
            getFocusModel().focus(newSelection.getFocus());
            scrollTo(Math.max(newSelection.row() - 1, 0));
        }
    }

    @EventListener
    public void onSetPageRanges(SetPageRangesRequest event) {
        getItems().forEach(i -> i.pageSelection.set(event.range()));
    }

    @EventListener
    public void onSetPace(SetPaceRequest event) {
        getItems().forEach(i -> i.pace.set(event.pace()));
    }

    @EventListener
    public void showPasswordFieldPopup(ShowPasswordFieldPopupRequest request) {
        Scene scene = this.getScene();
        if (scene != null) {
            Window owner = scene.getWindow();
            if (owner != null && owner.isShowing()) {
                Region reqNode = request.requestingNode();
                double anchorX, anchorY;
                if (reqNode == this) {
                    anchorX = owner.getX() + owner.getWidth() / 2.0;
                    anchorY = localToScreen(getLayoutBounds()).getCenterY();
                } else {
                    Point2D nodeCoord = reqNode.localToScene(reqNode.getWidth() / 2, reqNode.getHeight() / 1.5);
                    anchorX = Math.round(owner.getX() + scene.getX() + nodeCoord.getX() + 2);
                    anchorY = Math.round(owner.getY() + scene.getY() + nodeCoord.getY() + 2);
                }
                passwordPopup.showFor(this, anchorX, anchorY, request.pdfDescriptors());
            }
        }
    }

    private void copySelectedToClipboard() {
        ClipboardContent content = new ClipboardContent();
        writeContent(getSelectionModel().getSelectedItems().stream()
                .map(item -> item.descriptor().getFile().getAbsolutePath() + ", " + item.descriptor().getFile().length()
                        + ", " + item.descriptor().pages().getValue()).collect(Collectors.toList())).to(content);
        Clipboard.getSystemClipboard().setContent(content);
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put(defaultString(getId()) + "input.size", Integer.toString(getItems().size()));
        IntStream.range(0, getItems().size()).forEach(i -> {
            SelectionTableRowData current = getItems().get(i);
            String id = defaultString(getId());
            data.put(id + "input." + i, current.descriptor().getFile().getAbsolutePath());
            if (app().persistentSettings().get(BooleanPersistentProperty.SAVE_PWD_IN_WORKSPACE)) {
                data.put(id + "input.password.enc" + i, encrypt(current.descriptor().getPassword()));
            }
            data.put(id + "input.range." + i, defaultString(current.pageSelection.get()));
            data.put(id + "input.step." + i, defaultString(current.pace.get()));
            data.put(id + "input.reverse." + i, Boolean.toString(current.reverse.get()));
        });
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        onClear(null);
        int size = ofNullable(data.get(defaultString(getId()) + "input.size")).map(Integer::valueOf).orElse(0);
        if (size > 0) {
            PdfLoadRequest loadEvent = new PdfLoadRequest(toolBinding());
            List<SelectionTableRowData> items = new ArrayList<>();
            IntStream.range(0, size).forEach(i -> {
                String id = defaultString(getId());
                ofNullable(data.get(id + "input." + i)).ifPresent(f -> {
                    PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptor(new File(f),
                            ofNullable(data.get(id + "input.password.enc" + i)).map(EncryptionUtils::decrypt)
                                    .orElseGet(() -> data.get(defaultString(getId()) + "input.password." + i)));
                    loadEvent.add(descriptor);
                    SelectionTableRowData row = new SelectionTableRowData(descriptor);
                    row.pageSelection.set(data.get(id + "input.range." + i));
                    row.pace.set(data.get(id + "input.step." + i));
                    row.reverse.set(Boolean.parseBoolean(data.get(id + "input.reverse." + i)));
                    items.add(row);
                });
            });
            getItems().addAll(items);
            eventStudio().broadcast(loadEvent);
        }

    }

    public IntegerProperty getHoverIndex() {
        return hoverIndex;
    }

    private void dragScroll() {
        var scrollBar = getVerticalScrollbar();

        if (scrollBar != null) {
            var newValue = scrollBar.getValue() + scrollDirection;

            newValue = Math.min(newValue, 1.0);
            newValue = Math.max(newValue, 0.0);

            scrollBar.setValue(newValue);
        }
    }

    private ScrollBar getVerticalScrollbar() {
        ScrollBar verticalScrollBar = null;

        for (var node : lookupAll(".scroll-bar")) {
            if (node instanceof ScrollBar bar) {
                if (bar.getOrientation().equals(Orientation.VERTICAL)) {
                    verticalScrollBar = bar;
                }
            }
        }

        return verticalScrollBar;
    }

    private void autoscrollIfNeeded(DragEvent evt) {
        var hotRegion = (Region) lookup(".clipped-container");

        if (hotRegion.getBoundsInLocal().getWidth() < 1) {
            hotRegion = this;
            if (hotRegion.getBoundsInLocal().getWidth() < 1) {
                stopAutoScrollIfNeeded(evt);
                return;
            }
        }

        var yOffset = 0.0;
        var delta = evt.getSceneY() - hotRegion.localToScene(0, 0).getY();
        var proximity = 50.0;

        if (delta < proximity) {
            yOffset = -(proximity - delta);
        }

        delta = hotRegion.localToScene(0, 0).getY() + hotRegion.getHeight() - evt.getSceneY();

        if (delta < proximity) {
            yOffset = proximity - delta;
        }

        if (yOffset != 0) {
            autoscroll(yOffset);
        } else {
            stopAutoScrollIfNeeded(evt);
        }
    }

    private void stopAutoScrollIfNeeded(DragEvent evt) {
        scrollTimeline.stop();
    }

    private void autoscroll(double yOffset) {
        if (yOffset > 0) {
            scrollDirection = 1.0 / getItems().size();
        } else {
            scrollDirection = -1.0 / getItems().size();
        }
        scrollTimeline.play();
    }

}
