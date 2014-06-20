/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
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
package org.pdfsam.ui.io;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.input.DragEvent;
import javafx.scene.input.TransferMode;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.TaskParametersBuildStep;
import org.pdfsam.support.validation.Validator;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.sejda.conversion.DirectoryOutputAdapter;
import org.sejda.model.parameter.base.MultipleOutputTaskParameters;

/**
 * Component letting the user select an existing directory
 * 
 * @author Andrea Vacondio
 * 
 */
public class BrowsableDirectoryField extends BrowsableField implements
        TaskParametersBuildStep<MultipleOutputTaskParameters> {

    private BrowseEventHandler handler = new BrowseEventHandler();

    public BrowsableDirectoryField(boolean allowBlankString) {
        setBrowseWindowTitle(DefaultI18nContext.getInstance().i18n("Select a directory"));
        Validator<String> validator = Validators.newExistingDirectoryString();
        if (allowBlankString) {
            validator = Validators.decorateAsValidBlankString(validator);
        }
        getTextField().setValidator(validator);
        getTextField().setErrorMessage(DefaultI18nContext.getInstance().i18n("Select an existing directory"));
        getTextField().setPromptText(DefaultI18nContext.getInstance().i18n("Select a directory"));
        getBrowseButton().setOnAction(handler);
        getTextField().setOnAction(handler);
        setOnDragOver(e -> dragConsume(e, this.onDragOverConsumer()));
        setOnDragDropped(e -> dragConsume(e, this.onDragDropped()));
    }

    /**
     * {@link EventHandler} opening the {@link javafx.stage.DirectoryChooser} and letting the user select the input directory populating the ValidableTextField.
     * 
     * @author Andrea Vacondio
     * 
     */
    private class BrowseEventHandler implements EventHandler<ActionEvent> {

        public void handle(ActionEvent event) {
            RememberingLatestDirectoryChooserWrapper fileChooser = FileChoosers
                    .getDirectoryChooser(getBrowseWindowTitle());
            String currentSelection = getTextField().getText();
            if (isNotBlank(currentSelection)) {
                Path path = Paths.get(currentSelection);
                if (Files.exists(path)) {
                    fileChooser.setInitialDirectory(path.toFile());
                }
            }
            setTextFromFile(fileChooser.showDialog(getTextField().getScene().getWindow()));
        }
    }

    @Override
    void setTextFromFile(File inputFile) {
        if (inputFile != null) {
            if (inputFile.isDirectory()) {
                getTextField().setText(inputFile.getAbsolutePath());
            } else {
                getTextField().setText(inputFile.getParent());
            }
            getTextField().validate();
        }
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

    private Consumer<DragEvent> onDragDropped() {
        return (DragEvent e) -> {
            e.getDragboard().getFiles().stream().filter(f -> f.isDirectory()).findFirst()
                    .ifPresent((file) -> setTextFromFile(file));
        };
    }

    public void apply(Optional<? extends MultipleOutputTaskParameters> params, Consumer<String> onError) {
        getTextField().validate();
        if (getTextField().getValidationState() == ValidationState.INVALID) {
            onError.accept(DefaultI18nContext.getInstance().i18n("The selected output directory is invalid"));
        } else {
            params.ifPresent(p -> p.setOutput(new DirectoryOutputAdapter(getTextField().getText())
                    .getPdfDirectoryOutput()));
        }
    }
}
