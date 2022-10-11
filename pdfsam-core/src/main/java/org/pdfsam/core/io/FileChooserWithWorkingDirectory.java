/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28 sep 2022
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.core.io;

import javafx.stage.FileChooser;
import javafx.stage.Window;
import org.pdfsam.model.io.FileType;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;

import static java.util.Arrays.stream;
import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static java.util.function.Predicate.not;
import static org.pdfsam.core.context.ApplicationContext.app;

/**
 * A FileChooser that will open pointing to the latest know working directory and can be configured to open single/multiple files or show a save dialog
 *
 * @author Andrea Vacondio
 */
public class FileChooserWithWorkingDirectory {
    private final FileChooser wrapped = new FileChooser();

    FileChooserWithWorkingDirectory() {
        app().runtimeState().workingPath().subscribe(p -> wrapped.setInitialDirectory(p.map(Path::toFile).orElse(null)));
    }

    final void setTitle(String value) {
        wrapped.setTitle(value);
    }

    public final void setInitialFileName(String value) {
        wrapped.setInitialFileName(value);
    }

    public final void setInitialDirectory(Path value) {
        wrapped.setInitialDirectory(ofNullable(value).map(Path::toFile).orElse(null));
    }

    private void sanitizeInitialDirectory() {
        if (ofNullable(wrapped.getInitialDirectory()).map(File::toPath).filter(not(Files::isDirectory)).isPresent()) {
            wrapped.setInitialDirectory(null);
        }
    }

    /**
     * Shows a dialog to select multiple files
     *
     * @param ownerWindow
     * @return
     */
    public List<Path> showOpenMultipleDialog(Window ownerWindow) {
        sanitizeInitialDirectory();
        List<Path> selected = ofNullable(wrapped.showOpenMultipleDialog(ownerWindow)).stream()
                .flatMap(Collection::stream).map(File::toPath).filter(Files::isRegularFile).toList();
        if (!selected.isEmpty()) {
            updateWorkingPath(selected.get(0));
        }
        return selected;
    }

    void setFileTypes(FileType... filters) {
        wrapped.getExtensionFilters().setAll(stream(filters).map(FileType::getFilter).toList());
    }

    private Path updateWorkingPath(Path path) {
        if (nonNull(path)) {
            app().runtimeState().workingPath(path);
        }
        return path;
    }

    private Path singleFileDialog(File file) {
        return ofNullable(file).map(File::toPath).filter(f -> !Files.isDirectory(f)).map(this::updateWorkingPath)
                .orElse(null);

    }

    /**
     * Shows a dialog to select/open a single file
     *
     * @param ownerWindow
     * @return
     */
    public Path showOpenSingleDialog(Window ownerWindow) {
        sanitizeInitialDirectory();
        return singleExistingFileDialog(wrapped.showOpenDialog(ownerWindow));
    }

    private Path singleExistingFileDialog(File file) {
        return ofNullable(file).map(File::toPath).filter(Files::isRegularFile).map(this::updateWorkingPath)
                .orElse(null);

    }

    /**
     * Shows a dialog to select/save a single file
     *
     * @param ownerWindow
     * @return
     */
    public Path showSaveDialog(Window ownerWindow) {
        sanitizeInitialDirectory();
        return singleFileDialog(wrapped.showSaveDialog(ownerWindow));
    }

    /**
     * Shows a dialog to select a single file or multiple files
     *
     * @param ownerWindow
     * @param multiple
     * @return
     */
    public List<Path> showOpenDialog(Window ownerWindow, boolean multiple) {
        if (!multiple) {
            return List.of(showOpenSingleDialog(ownerWindow));
        }
        return showOpenMultipleDialog(ownerWindow);
    }

}
