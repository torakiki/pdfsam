/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/feb/2014
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
package org.pdfsam.ui.io;

import static java.util.Optional.ofNullable;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.nio.file.Files;
import java.util.List;

import javafx.collections.ObservableList;
import javafx.stage.FileChooser;
import javafx.stage.FileChooser.ExtensionFilter;
import javafx.stage.Window;

/**
 * Wrapper around a {@link FileChooser} thats remembers its latest working directory. Subsequent openings will have initial directory set to it.
 * 
 * @author Andrea Vacondio
 *
 */
public class RememberingLatestFileChooserWrapper extends BaseRememberingLatestChooser {
    private FileChooser wrapped = new FileChooser();

    public RememberingLatestFileChooserWrapper() {
        eventStudio().addAnnotatedListeners(this);
    }

    final void setTitle(String value) {
        wrapped.setTitle(value);
    }

    @Override
    public final void setInitialDirectory(File value) {
        wrapped.setInitialDirectory(value);
    }

    public final void setInitialFileName(String value) {
        wrapped.setInitialFileName(value);
    }

    public List<File> showOpenMultipleDialog(Window ownerWindow) {
        sanitizeInitialDirectory();
        List<File> selected = wrapped.showOpenMultipleDialog(ownerWindow);
        if (selected != null && !selected.isEmpty()) {
            notifyNewLatestDirectory(selected.get(0));
        }
        return selected;
    }

    private void sanitizeInitialDirectory() {
        if (ofNullable(wrapped.getInitialDirectory()).map(File::toPath).filter(f -> !Files.isDirectory(f))
                .isPresent()) {
            wrapped.setInitialDirectory(null);
        }
    }

    /**
     * Shows the file chooser dialog of the given type
     * 
     * @param type
     * @return the selected file or null
     */
    public File showDialog(OpenType type) {
        return showDialog(null, type);
    }

    /**
     * Shows the file chooser dialog of the given type
     * 
     * @param ownerWindow
     *            the window owning the dialog
     * @param type
     * @return the selected file or null
     */
    public File showDialog(Window ownerWindow, OpenType type) {
        File selected = null;
        switch (type) {
        case SAVE:
            sanitizeInitialDirectory();
            selected = wrapped.showSaveDialog(ownerWindow);
            break;
        default:
            sanitizeInitialDirectory();
            selected = wrapped.showOpenDialog(ownerWindow);
        }
        notifyNewLatestDirectory(selected);
        return selected;
    }

    private void notifyNewLatestDirectory(File selected) {
        if (selected != null && selected.isFile()) {
            eventStudio().broadcast(new SetLatestDirectoryEvent(selected.getParentFile()));
        }
    }

    ObservableList<ExtensionFilter> getExtensionFilters() {
        return wrapped.getExtensionFilters();
    }

    /**
     * Possible type of open dialogs
     * 
     * @author Andrea Vacondio
     *
     */
    public static enum OpenType {
        OPEN,
        SAVE;
    }
}
