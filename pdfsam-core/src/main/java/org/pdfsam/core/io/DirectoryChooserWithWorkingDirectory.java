/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28 sep 2022
 * Copyright 2022 by Sober Lemur S.r.l. (info@pdfsam.org).
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

import javafx.stage.DirectoryChooser;
import javafx.stage.Window;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static java.util.function.Predicate.not;
import static org.pdfsam.core.context.ApplicationContext.app;

/**
 * A DirectoryChooser that will open pointing to the latest know working directory
 *
 * @author Andrea Vacondio
 */
public class DirectoryChooserWithWorkingDirectory {
    private final DirectoryChooser wrapped = new DirectoryChooser();

    DirectoryChooserWithWorkingDirectory() {
        app().runtimeState().workingPath().subscribe(p -> wrapped.setInitialDirectory(p.map(Path::toFile).orElse(null)));
    }

    final void setTitle(String value) {
        wrapped.setTitle(value);
    }

    public final void setInitialDirectory(Path value) {
        wrapped.setInitialDirectory(ofNullable(value).map(Path::toFile).orElse(null));
    }

    public Path showDialog(Window ownerWindow) {
        if (ofNullable(wrapped.getInitialDirectory()).map(File::toPath).filter(not(Files::isDirectory)).isPresent()) {
            wrapped.setInitialDirectory(null);
        }
        Path selected = ofNullable(wrapped.showDialog(ownerWindow)).map(File::toPath).filter(Files::isDirectory)
                .orElse(null);
        if (nonNull(selected)) {
            app().runtimeState().workingPath(selected);
        }
        return selected;
    }
}
