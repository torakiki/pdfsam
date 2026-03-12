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
import javafx.scene.input.DragEvent;
import javafx.scene.input.TransferMode;
import org.pdfsam.core.io.Choosers;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Consumer;

import static java.util.Objects.nonNull;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Component letting the user select an existing directory
 * 
 * @author Andrea Vacondio
 * 
 */
public class BrowsableDirectoryField extends BrowsableField {

    public BrowsableDirectoryField() {
        setBrowseWindowTitle(i18n().tr("Select a directory"));
        getTextField().setErrorMessage(i18n().tr("Select an existing directory"));
        setFieldPromptAndAccessibleText(i18n().tr("Select a directory"));
        setBrowseButtonAccessibleText(i18n().tr("Browse for directory"));
        var handler = new BrowseEventHandler();
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

        @Override
        public void handle(ActionEvent event) {
            var directoryChooser = Choosers.directoryChooser(getBrowseWindowTitle());
            String currentSelection = getTextField().getText();
            if (isNotBlank(currentSelection)) {
                var path = Paths.get(currentSelection);
                //if not absolute, resolve against working path
                if (!path.isAbsolute()) {
                    path = app().runtimeState().workingPathValue().map(w -> w.resolve(currentSelection)).orElse(path);
                }
                if (Files.exists(path)) {
                    directoryChooser.setInitialDirectory(path);
                }
            }
            setTextFromFile(directoryChooser.showDialog(getTextField().getScene().getWindow()));
        }
    }

    @Override
    void setTextFromFile(File inputFile) {
        if (nonNull(inputFile)) {
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
        return (DragEvent e) -> e.acceptTransferModes(TransferMode.COPY_OR_MOVE);
    }

    private Consumer<DragEvent> onDragDropped() {
        return (DragEvent e) -> {
            e.getDragboard().getFiles().stream().filter(File::isDirectory).findFirst().ifPresent(this::setTextFromFile);
            e.setDropCompleted(true);
        };
    }
}
