/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/mag/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.support.RequireUtils.requireNotNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.function.Consumer;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.WeakChangeListener;
import javafx.geometry.Pos;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfDocumentDescriptorProvider;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.TaskParametersBuildStep;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.event.OpenFileRequest;
import org.pdfsam.ui.event.SetDestinationRequest;
import org.pdfsam.ui.event.ShowPdfDescriptorRequest;
import org.pdfsam.ui.io.BrowsableFileField;
import org.pdfsam.ui.selection.LoadingStatusIndicator;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.sejda.model.parameter.base.SinglePdfSourceTaskParameters;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Panel letting the user select a single input PDF document
 * 
 * @author Andrea Vacondio
 *
 */
public class SingleSelectionPane<T extends SinglePdfSourceTaskParameters> extends VBox implements ModuleOwned,
        TaskParametersBuildStep<T>, PdfDocumentDescriptorProvider {

    private String ownerModule = StringUtils.EMPTY;
    private BrowsableFileField field = new BrowsableFileField(FileType.PDF);
    private Label pages = new Label();
    private PdfDocumentDescriptor descriptor;
    private LoadingStatusIndicator encryptionIndicator;
    private ChangeListener<PdfDescriptorLoadingStatus> onDescriptorLoaded = (o, oldVal, newVal) -> {
        Platform.runLater(() -> {
            encryptionIndicator.setLoadingStatus(newVal);
            if (newVal == PdfDescriptorLoadingStatus.LOADED
                    || newVal == PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION) {
                pages.setText(DefaultI18nContext.getInstance().i18n("Pages: {0}",
                        Integer.toString(descriptor.pagesPropery().get())));
            } else {
                pages.setText("");
            }
        });
    };

    public SingleSelectionPane(String ownerModule) {
        super(5);
        this.ownerModule = defaultString(ownerModule);
        field.enforceValidation(true, false);
        field.getTextField().setPromptText(
                DefaultI18nContext.getInstance().i18n("Select or drag and drop the PDF you want to split"));
        encryptionIndicator = new LoadingStatusIndicator(this, this.ownerModule);
        field.setGraphic(encryptionIndicator);
        HBox topRow = new HBox(5, field);
        HBox.setHgrow(field, Priority.ALWAYS);
        topRow.setAlignment(Pos.CENTER_LEFT);
        getChildren().addAll(topRow, pages);
        field.getTextField().setEditable(false);
        field.getTextField().validProperty().addListener((o, oldVal, newVal) -> {
            if (newVal == ValidationState.VALID) {
                if (descriptor != null) {
                    descriptor.invalidate();
                }
                PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(getOwnerModule());
                descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(new File(field.getTextField().getText()));
                descriptor.loadedProperty().addListener(new WeakChangeListener<>(onDescriptorLoaded));
                field.getTextField().getContextMenu().getItems().forEach(i -> i.setDisable(false));
                loadEvent.add(descriptor);
                eventStudio().broadcast(loadEvent);
            }
        });
        initContextMenu();
    }

    public void apply(T params, Consumer<String> onError) {
        requireNotNull(params, "Cannot set input on a null parameter instance");
        field.getTextField().validate();
        if (field.getTextField().getValidationState() == ValidationState.INVALID) {
            onError.accept(DefaultI18nContext.getInstance().i18n("The selected PDF document is invalid"));
        } else {
            params.setSource(descriptor.toPdfFileSource());
        }
    }

    private void initContextMenu() {
        MenuItem infoItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Document properties"),
                AwesomeIcon.INFO);
        infoItem.setOnAction(e -> Platform.runLater(() -> {
            eventStudio().broadcast(new ShowPdfDescriptorRequest(descriptor));
        }));

        MenuItem setDestinationItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Set output"),
                AwesomeIcon.PENCIL_SQUARE_ALT);
        setDestinationItem.setOnAction(e -> {
            eventStudio().broadcast(new SetDestinationRequest(descriptor.getFile().getParentFile()), getOwnerModule());
        });

        MenuItem openFileItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Open"), AwesomeIcon.FILE_ALT);
        openFileItem.setOnAction(e -> {
            eventStudio().broadcast(new OpenFileRequest(descriptor.getFile()));
        });

        MenuItem openFolderItem = createMenuItem(DefaultI18nContext.getInstance().i18n("Open Folder"),
                AwesomeIcon.FOLDER_OPEN);
        openFolderItem.setOnAction(e -> {
            eventStudio().broadcast(new OpenFileRequest(descriptor.getFile().getParentFile()));
        });

        field.getTextField().setContextMenu(
                new ContextMenu(setDestinationItem, new SeparatorMenuItem(), infoItem, openFileItem, openFolderItem));
    }

    private MenuItem createMenuItem(String text, AwesomeIcon icon) {
        MenuItem item = new MenuItem(text);
        AwesomeDude.setIcon(item, icon);
        item.setDisable(true);
        return item;
    }

    public String getOwnerModule() {
        return ownerModule;
    }

    public PdfDocumentDescriptor getPdfDocumentDescriptor() {
        return descriptor;
    }
}
