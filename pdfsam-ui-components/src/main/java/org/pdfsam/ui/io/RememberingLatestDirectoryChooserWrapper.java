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
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.nio.file.Files;

import javafx.stage.DirectoryChooser;
import javafx.stage.Window;

/**
 * Wrapper around a {@link DirectoryChooser} thats remembers its latest working directory. Subsequent openings will have initial directory set to it.
 * 
 * @author Andrea Vacondio
 *
 */
class RememberingLatestDirectoryChooserWrapper extends BaseRememberingLatestChooser {
    private DirectoryChooser wrapped = new DirectoryChooser();

    public RememberingLatestDirectoryChooserWrapper() {
        eventStudio().addAnnotatedListeners(this);
    }

    public final void setTitle(String value) {
        wrapped.setTitle(value);
    }

    @Override
    public final void setInitialDirectory(File value) {
        wrapped.setInitialDirectory(value);
    }

    public File showDialog(Window ownerWindow) {
        if (ofNullable(wrapped.getInitialDirectory()).map(File::toPath).filter(f -> !Files.isDirectory(f))
                .isPresent()) {
            wrapped.setInitialDirectory(null);
        }
        File selected = wrapped.showDialog(ownerWindow);
        if (selected != null && selected.isDirectory()) {
            eventStudio().broadcast(new SetLatestDirectoryEvent(selected));
        }
        return selected;
    }

}
