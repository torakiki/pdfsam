/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/mag/2014
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
package org.pdfsam.ui.selection.single;

import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import de.jensd.fx.glyphs.materialdesignicons.utils.MaterialDesignIconFactory;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;
import javafx.geometry.Insets;
import javafx.geometry.Point2D;
import javafx.scene.Scene;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Window;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.core.context.DefaultUserContext;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.tool.ToolBound;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfDocumentDescriptorProvider;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.core.support.EncryptionUtils;
import org.pdfsam.core.support.io.FileType;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.commons.NativeOpenFileRequest;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.pdfsam.ui.commons.ToggleChangeListener;
import org.pdfsam.ui.io.BrowsableFileField;
import org.pdfsam.ui.io.ChangedSelectedPdfVersionEvent;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper.OpenType;
import org.pdfsam.ui.selection.LoadingStatusIndicatorUpdater;
import org.pdfsam.ui.selection.PasswordFieldPopup;
import org.pdfsam.ui.selection.single.SingleSelectionPaneToolbar.SelectButton;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.workspace.RestorableView;

import java.io.File;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.ENCRYPTED;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.WITH_ERRORS;
import static org.pdfsam.pdf.PdfDocumentDescriptor.newDescriptor;
import static org.pdfsam.pdf.PdfDocumentDescriptor.newDescriptorNoPassword;
import static org.pdfsam.core.support.EncryptionUtils.encrypt;
import static org.pdfsam.ui.commons.SetDestinationRequest.requestDestination;
import static org.pdfsam.ui.commons.SetDestinationRequest.requestFallbackDestination;

/**
 * Panel letting the user select a single input PDF document
 *
 * @author Andrea Vacondio
 */
public class SingleSelectionPane extends VBox implements ToolBound, PdfDocumentDescriptorProvider, RestorableView {

    private String ownerModule = StringUtils.EMPTY;
    private BrowsableFileField field;
    private Label details = new Label();
    private PdfDocumentDescriptor descriptor;
    private PasswordFieldPopup passwordPopup;
    private Label encryptionIndicator = new Label();
    private MenuItem removeSelected;

    private Consumer<PdfDocumentDescriptor> onLoaded = d -> {
        eventStudio().broadcast(requestFallbackDestination(d.getFile(), toolBinding()), toolBinding());
        eventStudio().broadcast(new ChangedSelectedPdfVersionEvent(d.getVersion()), toolBinding());
    };

    private Consumer<PdfDocumentDescriptor> detailsUpdate = d -> {
        PdfDescriptorLoadingStatus status = d.loadingStatus().getValue();
        if (status == PdfDescriptorLoadingStatus.LOADED
                || status == PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION) {
            details.setText(i18n().tr("Pages: {0}, PDF Version: {1}", Integer.toString(d.pages().getValue()),
                    d.getVersionString()));
        } else if (status == PdfDescriptorLoadingStatus.LOADING) {
            details.setText(i18n().tr("Loading..."));
        } else {
            details.setText("");
        }
    };

    private Consumer<PdfDocumentDescriptor> encryptionIndicatorUpdate = new Consumer<>() {
        private LoadingStatusIndicatorUpdater updater = new LoadingStatusIndicatorUpdater(encryptionIndicator);

        @Override
        public void accept(PdfDocumentDescriptor t) {
            updater.accept(t.loadingStatus().getValue());
        }
    };

    private ChangeListener<PdfDescriptorLoadingStatus> onLoadingStatusChange = (o, oldVal, newVal) -> {
        if (descriptor != null & descriptor.hasReferences()) {
            encryptionIndicatorUpdate.andThen(detailsUpdate).andThen(d -> {
                PdfDescriptorLoadingStatus status = d.loadingStatus().getValue();
                if (status == PdfDescriptorLoadingStatus.LOADED
                        || status == PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION) {
                    onLoaded.accept(d);
                }
            }).accept(descriptor);
        }
    };

    private ToggleChangeListener<? super ValidationState> onValidState = new ToggleChangeListener<>() {

        @Override
        public void onChanged(ObservableValue<? extends ValidationState> observable, ValidationState oldValue,
                ValidationState newVal) {
            if (newVal == ValidationState.VALID) {
                initializeFor(newDescriptorNoPassword(new File(field.getTextField().getText())));
            } else {
                reset();
            }
        }
    };

    public SingleSelectionPane(String ownerModule) {
        this.getStyleClass().add("single-selection-pane");
        this.ownerModule = defaultString(ownerModule);
        this.details.getStyleClass().add("-pdfsam-selection-details");
        SelectButton selectButton = new SelectButton(toolBinding());
        field = new BrowsableFileField(FileType.PDF, OpenType.OPEN, selectButton);
        field.enforceValidation(true, false);
        passwordPopup = new PasswordFieldPopup(this.ownerModule);
        encryptionIndicator.getStyleClass().addAll("encryption-status");
        encryptionIndicator.addEventFilter(MouseEvent.MOUSE_CLICKED, e -> {
            if (descriptor.loadingStatus().getValue() == ENCRYPTED) {
                showPasswordFieldPopup();
            } else if (descriptor.loadingStatus().getValue() == WITH_ERRORS) {
                eventStudio().broadcast(ShowStageRequest.INSTANCE, "LogStage");
            }
        });
        HBox.setMargin(encryptionIndicator, new Insets(0, 0, 0, 2));
        field.setGraphic(encryptionIndicator);
        field.getStyleClass().add("single-selection-top");
        HBox.setHgrow(field, Priority.ALWAYS);
        getChildren().addAll(new SingleSelectionPaneToolbar(selectButton, toolBinding()), field, details);
        field.getTextField().validProperty().addListener(onValidState);
        initContextMenu();
        eventStudio().addAnnotatedListeners(this);
    }

    private void initializeFor(PdfDocumentDescriptor docDescriptor) {
        invalidateDescriptor();
        PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(toolBinding());
        descriptor = docDescriptor;
        descriptor.loadingStatus().addListener(new WeakChangeListener<>(onLoadingStatusChange));
        setContextMenuDisable(false);
        loadEvent.add(descriptor);
        eventStudio().broadcast(loadEvent);
    }

    private void reset() {
        invalidateDescriptor();
        setContextMenuDisable(true);
        encryptionIndicator.setText("");
        details.setText("");
    }

    private void disableRemoveMenuItemIfNeeded() {
        removeSelected.setDisable(isEmpty(field.getTextField().getText()));
    }

    private void setContextMenuDisable(boolean value) {
        field.getTextField().getContextMenu().getItems().forEach(i -> i.setDisable(value));
        disableRemoveMenuItemIfNeeded();
    }

    private void invalidateDescriptor() {
        if (nonNull(descriptor)) {
            descriptor.releaseAll();
        }
    }

    private void showPasswordFieldPopup() {
        Scene scene = this.getScene();
        if (scene != null) {
            Window owner = scene.getWindow();
            if (owner != null && owner.isShowing()) {
                Point2D nodeCoord = encryptionIndicator.localToScene(encryptionIndicator.getWidth() / 2,
                        encryptionIndicator.getHeight() / 1.5);
                double anchorX = Math.round(owner.getX() + scene.getX() + nodeCoord.getX() + 2);
                double anchorY = Math.round(owner.getY() + scene.getY() + nodeCoord.getY() + 2);
                passwordPopup.showFor(this, descriptor, anchorX, anchorY);
            }
        }
    }

    @Override
    public PdfDocumentDescriptor getPdfDocumentDescriptor() {
        return descriptor;
    }

    protected BrowsableFileField getField() {
        return field;
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        if (descriptor != null) {
            data.put(defaultString(getId()) + "input", descriptor.getFile().getAbsolutePath());
            if (new DefaultUserContext().isSavePwdInWorkspaceFile()) {
                data.put(defaultString(getId()) + "input.password.enc", encrypt(descriptor.getPassword()));
            }
        }
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        getField().getTextField().setText(EMPTY);
        Optional.ofNullable(data.get(defaultString(getId()) + "input")).ifPresent(f -> {
            onValidState.disabled(true);
            getField().getTextField().setText(f);
            onValidState.disabled(false);
            initializeFor(newDescriptor(new File(f),
                    ofNullable(data.get(defaultString(getId()) + "input.password.enc")).map(EncryptionUtils::decrypt)
                            .orElseGet(() -> data.get(defaultString(getId()) + "input.password"))));
        });
    }

    /**
     * to perform when the document is loaded
     * 
     * @param onDescriptorLoaded
     */
    public void addOnLoaded(Consumer<PdfDocumentDescriptor> onDescriptorLoaded) {
        this.onLoaded = onDescriptorLoaded.andThen(this.onLoaded);
    }

    private void initContextMenu() {
        MenuItem infoItem = createMenuItem(i18n().tr("Document properties"), MaterialDesignIcon.INFORMATION_OUTLINE);
        infoItem.setAccelerator(new KeyCodeCombination(KeyCode.P, KeyCombination.ALT_DOWN));
        infoItem.setOnAction(
                e -> Platform.runLater(() -> eventStudio().broadcast(new ShowPdfDescriptorRequest(descriptor))));

        removeSelected = createMenuItem(i18n().tr("Remove"), MaterialDesignIcon.MINUS);
        removeSelected.setOnAction(e -> eventStudio().broadcast(new ClearModuleEvent(toolBinding()), toolBinding()));

        MenuItem setDestinationItem = createMenuItem(i18n().tr("Set destination"), MaterialDesignIcon.AIRPLANE_LANDING);
        setDestinationItem.setAccelerator(new KeyCodeCombination(KeyCode.O, KeyCombination.ALT_DOWN));
        setDestinationItem.setOnAction(
                e -> eventStudio().broadcast(requestDestination(descriptor.getFile(), toolBinding()), toolBinding()));

        MenuItem openFileItem = createMenuItem(i18n().tr("Open"), MaterialDesignIcon.FILE_PDF_BOX);
        openFileItem.setOnAction(e -> eventStudio().broadcast(new NativeOpenFileRequest(descriptor.getFile())));

        MenuItem openFolderItem = createMenuItem(i18n().tr("Open Folder"), MaterialDesignIcon.FOLDER_OUTLINE);
        openFolderItem.setOnAction(
                e -> eventStudio().broadcast(new NativeOpenFileRequest(descriptor.getFile().getParentFile())));

        field.getTextField().setContextMenu(
                new ContextMenu(setDestinationItem, new SeparatorMenuItem(), removeSelected, new SeparatorMenuItem(),
                        infoItem, openFileItem, openFolderItem));
    }

    @EventListener
    public void onClearSelected(ClearModuleEvent event) {
        field.getTextField().clear();
        disableRemoveMenuItemIfNeeded();
    }

    @EventListener
    public void onLoadDocumentsRequest(PdfLoadRequestEvent loadEvent) {
        loadEvent.getDocuments().stream().findFirst().map(PdfDocumentDescriptor::getFile)
                .ifPresent(field::setTextFromFile);
    }

    private MenuItem createMenuItem(String text, MaterialDesignIcon icon) {
        MenuItem item = new MenuItem(text);
        MaterialDesignIconFactory.get().setIcon(item, icon, "1.1em");
        item.setDisable(true);
        return item;
    }

    @Override
    @EventStation
    public String toolBinding() {
        return ownerModule;
    }

    public void setPromptText(String text) {
        field.getTextField().setPromptText(text);
    }
}
