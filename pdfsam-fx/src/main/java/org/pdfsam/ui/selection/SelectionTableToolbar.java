/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2013
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

import java.io.File;
import java.util.List;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.stage.FileChooser;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadCompletedEvent;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.ModuleOwnedButton;
import org.pdfsam.ui.io.FileChoosers;
import org.sejda.eventstudio.annotation.EventListener;

/**
 * Toolbar for the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTableToolbar extends ToolBar implements ModuleOwned {

    private String ownerModule = StringUtils.EMPTY;

    public SelectionTableToolbar(String ownerModule) {
        this.ownerModule = defaultString(ownerModule);
        getItems().addAll(new AddButton(ownerModule), new ClearButton(ownerModule), new RemoveButton(ownerModule),
                new MoveUpButton(ownerModule), new MoveDownButton(ownerModule));
        getStyleClass().add("selection-tool-bar");
    }

    public String getOwnerModule() {
        return ownerModule;
    }

    /**
     * Button displayed in the selection table toolbar which disables itself when the selection table is loading documents
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class SelectionToolbarButton extends ModuleOwnedButton {

        public SelectionToolbarButton(String ownerModule) {
            super(ownerModule);
            eventStudio().addAnnotatedListeners(this);
        }

        @EventListener
        public final void disableWhileLoadingDocuments(PdfLoadRequestEvent event) {
            // I'm still loading documents
            this.setDisable(true);
        }

        @EventListener
        public final void enableOnLoadDocumentsCompletion(PdfLoadCompletedEvent event) {
            // I'm done loading documents
            Platform.runLater(() -> this.setDisable(false));
        }

    }

    /**
     * Button to request the load of the pdf documents selected using a {@link FileChooser}
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class AddButton extends SelectionToolbarButton {

        public AddButton(String ownerModule) {
            super(ownerModule);
            setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Add documents to the table")));
            setText(DefaultI18nContext.getInstance().i18n("_Add"));
            setOnAction(this::loadDocuments);
            // TODO accelerators
            // TODO icon
        }

        public void loadDocuments(ActionEvent event) {
            FileChooser fileChooser = FileChoosers.getFileChooser(FileType.PDF,
                    DefaultI18nContext.getInstance().i18n("Select pdf documents to load"));
            List<File> chosenFiles = fileChooser.showOpenMultipleDialog(this.getScene().getWindow());
            if (chosenFiles != null && !chosenFiles.isEmpty()) {
                PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(getOwnerModule());
                chosenFiles.forEach(d -> loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(d)));
                eventStudio().broadcast(loadEvent, getOwnerModule());
                eventStudio().broadcast(loadEvent);
            }
        }
    }

    /**
     * Button to request that the selected rows are removed
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class RemoveButton extends ModuleOwnedButton {

        public RemoveButton(String ownerModule) {
            super(ownerModule);
            setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Removes selected documents")));
            setText(DefaultI18nContext.getInstance().i18n("_Remove"));
            setOnAction(this::removeSelected);
            setDisable(true);
            eventStudio().addAnnotatedListeners(this);
        }

        public void removeSelected(ActionEvent event) {
            eventStudio().broadcast(new RemoveSelectedEvent(), getOwnerModule());
        }

        @EventListener
        public void disableIfNoSelection(final SelectionChangedEvent event) {
            setDisable(event.isClearSelection());
        }
    }

    /**
     * Button to request the selection table to clear its data
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class ClearButton extends SelectionToolbarButton {
        public ClearButton(String ownerModule) {
            super(ownerModule);
            setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Removes every document")));
            setText(DefaultI18nContext.getInstance().i18n("_Clear"));
            setOnAction(this::clear);
            // TODO accelerators
            // TODO icon
        }

        public void clear(ActionEvent event) {
            eventStudio().broadcast(new ClearSelectionTableEvent(), getOwnerModule());
        }
    }

    /**
     * Base button for move selected rows actions
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class BaseMoveSelectedButton extends ModuleOwnedButton {

        private MoveType type;

        public BaseMoveSelectedButton(String ownerModule, MoveType type) {
            super(ownerModule);
            this.type = type;
            setDisable(true);
            setOnAction(this::moveOnClick);
            eventStudio().addAnnotatedListeners(this);
        }

        public void moveOnClick(ActionEvent event) {
            eventStudio().broadcast(new MoveSelectedEvent(type), getOwnerModule());
        }

        @EventListener
        public void disableIfCannotMoveDown(final SelectionChangedEvent event) {
            setDisable(!event.canMove(type));
        }
    }

    private static class MoveUpButton extends BaseMoveSelectedButton {

        public MoveUpButton(String ownerModule) {
            super(ownerModule, MoveType.UP);
            setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Moves up selected documents")));
            setText(DefaultI18nContext.getInstance().i18n("Move _Up"));
            // TODO accelerators
        }
    }

    private static class MoveDownButton extends BaseMoveSelectedButton {

        public MoveDownButton(String ownerModule) {
            super(ownerModule, MoveType.DOWN);
            setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Moves down selected documents")));
            setText(DefaultI18nContext.getInstance().i18n("Move _Down"));
            // TODO accelerators
        }
    }
}
