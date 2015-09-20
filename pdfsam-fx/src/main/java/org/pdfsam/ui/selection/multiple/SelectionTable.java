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

import static java.util.Arrays.stream;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.ui.commons.SetDestinationRequest.requestDestination;
import static org.pdfsam.ui.commons.SetDestinationRequest.requestFallbackDestination;
import static org.pdfsam.ui.selection.multiple.SelectionChangedEvent.clearSelectionEvent;
import static org.pdfsam.ui.selection.multiple.SelectionChangedEvent.select;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.commons.OpenFileRequest;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.pdfsam.ui.selection.PasswordFieldPopup;
import org.pdfsam.ui.selection.ShowPasswordFieldPopupRequest;
import org.pdfsam.ui.selection.multiple.move.MoveSelectedEvent;
import org.pdfsam.ui.selection.multiple.move.MoveType;
import org.pdfsam.ui.selection.multiple.move.SelectionAndFocus;
import org.pdfsam.ui.workspace.RestorableView;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import javafx.application.Platform;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.geometry.Point2D;
import javafx.scene.Scene;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TableView;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.TransferMode;
import javafx.stage.Window;

/**
 * Table displaying selected pdf documents
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTable extends TableView<SelectionTableRowData> implements ModuleOwned, RestorableView {
    private static final Logger LOG = LoggerFactory.getLogger(SelectionTable.class);
    private String ownerModule = StringUtils.EMPTY;
    private Label placeHolder = new Label(DefaultI18nContext.getInstance().i18n("Drag and drop PDF files here"));
    private PasswordFieldPopup passwordPopup;

    public SelectionTable(String ownerModule, SelectionTableColumn<?>... columns) {
        this(ownerModule, false, columns);
    }

    public SelectionTable(String ownerModule, boolean canDuplicateItems, SelectionTableColumn<?>... columns) {
        this.ownerModule = defaultString(ownerModule);
        setEditable(true);
        getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
        Arrays.stream(columns).forEach(c -> getColumns().add(c.getTableColumn()));
        setColumnResizePolicy(CONSTRAINED_RESIZE_POLICY);
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
        passwordPopup = new PasswordFieldPopup(this.ownerModule);
        initContextMenu(canDuplicateItems);
        eventStudio().addAnnotatedListeners(this);
    }

    private void initContextMenu(boolean canDuplicateItems) {
        MenuItem infoItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Document properties"),
                FontAwesomeIcon.INFO);
        infoItem.setOnAction(e -> Platform.runLater(() -> eventStudio().broadcast(
                new ShowPdfDescriptorRequest(getSelectionModel().getSelectedItem()))));

        MenuItem setDestinationItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Set destination"),
                FontAwesomeIcon.FILE_PDF_ALT);
        setDestinationItem.setOnAction(e -> eventStudio()
                .broadcast(requestDestination(getSelectionModel().getSelectedItem().getFile(), getOwnerModule()),
                        getOwnerModule()));

        MenuItem removeSelected = createMenuItem(DefaultI18nContext.getInstance().i18n("Remove"),
                FontAwesomeIcon.MINUS_SQUARE_ALT);
        removeSelected.setOnAction(e -> eventStudio().broadcast(new RemoveSelectedEvent(), getOwnerModule()));

        MenuItem moveTopSelected = createMenuItem(DefaultI18nContext.getInstance().i18n("Move to Top"),
                FontAwesomeIcon.ANGLE_DOUBLE_UP);
        moveTopSelected
                .setOnAction(e -> eventStudio().broadcast(new MoveSelectedEvent(MoveType.TOP), getOwnerModule()));

        MenuItem moveUpSelected = createMenuItem(DefaultI18nContext.getInstance().i18n("Move Up"), FontAwesomeIcon.ANGLE_UP);
        moveUpSelected.setOnAction(e -> eventStudio().broadcast(new MoveSelectedEvent(MoveType.UP), getOwnerModule()));

        MenuItem moveDownSelected = createMenuItem(DefaultI18nContext.getInstance().i18n("Move Down"),
                FontAwesomeIcon.ANGLE_DOWN);
        moveDownSelected.setOnAction(e -> eventStudio().broadcast(new MoveSelectedEvent(MoveType.DOWN),
                getOwnerModule()));

        MenuItem moveBottomSelected = createMenuItem(DefaultI18nContext.getInstance().i18n("Move to Bottom"),
                FontAwesomeIcon.ANGLE_DOUBLE_DOWN);
        moveBottomSelected.setOnAction(e -> eventStudio().broadcast(new MoveSelectedEvent(MoveType.BOTTOM),
                getOwnerModule()));

        MenuItem openFileItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Open"), FontAwesomeIcon.FILE_ALT);
        openFileItem.setOnAction(e -> eventStudio().broadcast(
                new OpenFileRequest(getSelectionModel().getSelectedItem().getFile())));

        MenuItem openFolderItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Open Folder"),
                FontAwesomeIcon.FOLDER_OPEN_ALT);
        openFolderItem.setOnAction(e -> eventStudio().broadcast(
                new OpenFileRequest(getSelectionModel().getSelectedItem().getFile().getParentFile())));

        setDestinationItem.setAccelerator(new KeyCodeCombination(KeyCode.O, KeyCombination.ALT_DOWN));
        removeSelected.setAccelerator(new KeyCodeCombination(KeyCode.DELETE));
        moveBottomSelected.setAccelerator(new KeyCodeCombination(KeyCode.END, KeyCombination.ALT_DOWN));
        moveDownSelected.setAccelerator(new KeyCodeCombination(KeyCode.DOWN, KeyCombination.ALT_DOWN));
        moveUpSelected.setAccelerator(new KeyCodeCombination(KeyCode.UP, KeyCombination.ALT_DOWN));
        moveTopSelected.setAccelerator(new KeyCodeCombination(KeyCode.HOME, KeyCombination.ALT_DOWN));
        infoItem.setAccelerator(new KeyCodeCombination(KeyCode.P, KeyCombination.ALT_DOWN));
        openFileItem.setAccelerator(new KeyCodeCombination(KeyCode.O, KeyCombination.SHORTCUT_DOWN));
        openFolderItem.setAccelerator(new KeyCodeCombination(KeyCode.O, KeyCombination.SHORTCUT_DOWN,
                KeyCombination.ALT_DOWN));

        eventStudio().add(SelectionChangedEvent.class, e -> {
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

        ContextMenu context = new ContextMenu(setDestinationItem, new SeparatorMenuItem(), removeSelected,
                moveTopSelected, moveUpSelected, moveDownSelected, moveBottomSelected);

        if (canDuplicateItems) {
            MenuItem duplicateItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Duplicate"),
                    FontAwesomeIcon.COPY);
            duplicateItem.setOnAction(e -> eventStudio().broadcast(new DuplicateSelectedEvent(), getOwnerModule()));

            duplicateItem.setAccelerator(new KeyCodeCombination(KeyCode.DIGIT2, KeyCombination.ALT_DOWN));
            eventStudio().add(SelectionChangedEvent.class, e -> duplicateItem.setDisable(e.isClearSelection()),
                    getOwnerModule());
            context.getItems().add(duplicateItem);
        }

        context.getItems().addAll(new SeparatorMenuItem(), infoItem, openFileItem, openFolderItem);
        setContextMenu(context);
    }

    private MenuItem createMenuItem(String text, FontAwesomeIcon icon) {
        MenuItem item = new MenuItem(text);
        GlyphsDude.setIcon(item, icon);
        item.setDisable(true);
        return item;
    }

    private void dragConsume(DragEvent e, Consumer<DragEvent> c) {
        if (e.getDragboard().hasFiles()) {
            c.accept(e);
        }
        e.consume();
    }

    private Consumer<DragEvent> onDragOverConsumer() {
        return (DragEvent e) -> e.acceptTransferModes(TransferMode.COPY_OR_MOVE);
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
            final PdfLoadRequestEvent<SelectionTableRowData> loadEvent = new PdfLoadRequestEvent<>(getOwnerModule());
            getFilesFromDragboard(e.getDragboard()).filter(f -> FileType.PDF.matches(f.getName()))
                    .map(SelectionTableRowData::new).forEach(loadEvent::add);
            if (!loadEvent.getDocuments().isEmpty()) {
                eventStudio().broadcast(loadEvent, getOwnerModule());
            }
            e.setDropCompleted(true);
        };
    }

    private Stream<File> getFilesFromDragboard(Dragboard board) {
        List<File> files = board.getFiles();
        if (files.size() == 1 && files.get(0).isDirectory()) {
            return stream(files.get(0).listFiles()).sorted();
        }
        return files.stream();
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
    public void onDuplicate(final DuplicateSelectedEvent event) {
        LOG.trace("Duplicating selected items");
        getSelectionModel().getSelectedItems().forEach(i -> getItems().add(i.retain()));
    }

    @EventListener
    public void onClear(final ClearSelectionTableEvent event) {
        getItems().forEach(d -> d.invalidate());
        getSelectionModel().clearSelection();
        getItems().clear();
    }

    @EventListener
    public void onRemoveSelected(RemoveSelectedEvent event) {
        SortedSet<Integer> indices = new TreeSet<>(Collections.reverseOrder());
        indices.addAll(getSelectionModel().getSelectedIndices());
        LOG.trace("Removing {} items", indices.size());
        indices.forEach(i -> getItems().remove(i.intValue()).release());
        requestFocus();
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

    @EventListener
    public void showPasswordFieldPopup(ShowPasswordFieldPopupRequest request) {
        Scene scene = this.getScene();
        if (scene != null) {
            Window owner = scene.getWindow();
            if (owner != null && owner.isShowing()) {
                Point2D nodeCoord = request.getRequestingNode().localToScene(
                        request.getRequestingNode().getWidth() / 2, request.getRequestingNode().getHeight() / 1.5);
                double anchorX = Math.round(owner.getX() + scene.getX() + nodeCoord.getX() + 2);
                double anchorY = Math.round(owner.getY() + scene.getY() + nodeCoord.getY() + 2);
                passwordPopup.showFor(this, request.getPdfDescriptor(), anchorX, anchorY);
            }
        }
    }

    public void saveStateTo(Map<String, String> data) {
        data.put(defaultString(getId()) + "input.size", Integer.toString(getItems().size()));
        IntStream.range(0, getItems().size()).forEach(i -> {
            SelectionTableRowData current = getItems().get(i);
            data.put(defaultString(getId()) + "input." + i, current.getFile().getAbsolutePath());
            data.put(defaultString(getId()) + "input.password." + i, current.getPassword());
            data.put(defaultString(getId()) + "input.range." + i, current.getPageSelection());
        });
    }

    public void restoreStateFrom(Map<String, String> data) {
        onClear(null);
        int size = Optional.ofNullable(data.get(defaultString(getId()) + "input.size")).map(Integer::valueOf).orElse(0);
        if (size > 0) {
            PdfLoadRequestEvent<SelectionTableRowData> loadEvent = new PdfLoadRequestEvent<>(getOwnerModule());
            IntStream.range(0, size).forEach(
                    i -> {
                        Optional.ofNullable(data.get(defaultString(getId()) + "input." + i)).ifPresent(
                                f -> {
                                    SelectionTableRowData current = new SelectionTableRowData(new File(f), data
                                            .get(defaultString(getId()) + "input.password." + i));
                                    current.setPageSelection(data.get(defaultString(getId()) + "input.range." + i));
                                    loadEvent.add(current);
                                });
                    });
            getItems().addAll(loadEvent.getDocuments());
            eventStudio().broadcast(loadEvent);
        }

    }
}
