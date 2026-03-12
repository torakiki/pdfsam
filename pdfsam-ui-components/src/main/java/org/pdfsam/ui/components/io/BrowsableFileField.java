/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
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
package org.pdfsam.ui.components.io;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Button;
import javafx.scene.input.DragEvent;
import javafx.scene.input.TransferMode;
import org.apache.commons.lang3.ObjectUtils;
import org.pdfsam.core.io.Choosers;
import org.pdfsam.core.support.validation.Validator;
import org.pdfsam.core.support.validation.Validators;
import org.pdfsam.model.io.FileType;
import org.pdfsam.model.io.OpenType;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.apache.commons.lang3.StringUtils.trim;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Component letting the user select a File of an expected type. By default, no validation is enforced and the filetype is used only in the file chooser but the component provides a
 * method to initialize validation.
 *
 * @author Andrea Vacondio
 */
public class BrowsableFileField extends BrowsableField {

    private FileType fileType = FileType.ALL;
    private OpenType openType = OpenType.OPEN;
    private final BrowseEventHandler handler = new BrowseEventHandler();

    public BrowsableFileField(FileType fileType, OpenType openType) {
        this.init(fileType, openType);
    }

    public BrowsableFileField(FileType fileType, OpenType openType, Button browseButton) {
        super(browseButton);
        this.init(fileType, openType);
    }

    private void init(FileType fileType, OpenType openType) {
        setBrowseWindowTitle(i18n().tr("Select a file"));
        getBrowseButton().setOnAction(handler);
        getTextField().setOnAction(handler);
        this.fileType = ObjectUtils.getIfNull(fileType, FileType.ALL);
        this.openType = ObjectUtils.getIfNull(openType, OpenType.OPEN);
        if (FileType.ALL != fileType) {
            var prompt = String.format("%s: %s", i18n().tr("Select a file"), fileType.getFilter().getExtensions());
            setFieldPromptAndAccessibleText(prompt);
        } else {
            setFieldPromptAndAccessibleText(i18n().tr("Select a file"));
        }
        setBrowseButtonAccessibleText(i18n().tr("Browse for {0}", getTextField().getPromptText()));
        setOnDragOver(e -> dragConsume(e, this.onDragOverConsumer()));
        setOnDragDropped(e -> dragConsume(e, this.onDragDropped()));
    }

    /**
     * Configure validation for the field
     */
    public void enforceValidation(boolean selectedFileMustExists, boolean allowEmptyString) {
        Validator<String> validator = Validators.fileType(fileType, selectedFileMustExists);
        if (allowEmptyString) {
            validator = Validators.validEmpty(validator);
        }
        getTextField().setValidator(validator);
        getTextField().setErrorMessage(buildErrorMessage(selectedFileMustExists));
    }

    private String buildErrorMessage(boolean selectedFileMustExists) {
        String errorMessage = selectedFileMustExists ? i18n().tr("The selected file must exist. ") : "";
        if (FileType.ALL != fileType) {
            errorMessage += i18n().tr("Allowed extensions are {0}", fileType.getFilter().getDescription());
        }
        return trim(errorMessage);
    }

    /**
     * {@link EventHandler} opening the {@link javafx.stage.FileChooser} and letting the user select the input file populating the ValidableTextField.
     *
     * @author Andrea Vacondio
     */
    private class BrowseEventHandler implements EventHandler<ActionEvent> {

        @Override
        public void handle(ActionEvent event) {
            var fileChooser = Choosers.fileChooser(getBrowseWindowTitle(), fileType);
            String currentSelection = getTextField().getText();
            if (isNotBlank(currentSelection)) {
                var path = Paths.get(currentSelection);
                //if not absolute, resolve against working path
                if (!path.isAbsolute()) {
                    path = app().runtimeState().workingPathValue().map(w -> w.resolve(currentSelection)).orElse(path);
                }
                if (Files.exists(path)) {
                    fileChooser.setInitialDirectory(path.getParent());
                    fileChooser.setInitialFileName(path.getFileName().toString());
                }
            }
            switch (openType) {
            case SAVE -> setTextFromFile(fileChooser.showSaveDialog(getTextField().getScene().getWindow()));
            case OPEN -> setTextFromFile(fileChooser.showOpenSingleDialog(getTextField().getScene().getWindow()));
            }

        }
    }

    @Override
    public void setTextFromFile(File inputFile) {
        if (inputFile != null) {
            getTextField().setText(inputFile.getAbsolutePath());
            getTextField().validate();
        }
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

    private Consumer<DragEvent> onDragDropped() {
        return (DragEvent e) -> {
            e.getDragboard().getFiles().stream().filter(f -> fileType.matches(f.getName())).findFirst()
                    .ifPresent(this::setTextFromFile);
            e.setDropCompleted(true);
        };
    }
}
