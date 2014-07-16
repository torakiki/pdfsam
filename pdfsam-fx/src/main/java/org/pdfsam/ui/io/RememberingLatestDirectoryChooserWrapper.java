/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/feb/2014
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
package org.pdfsam.ui.io;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;

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

    public final void setTitle(String value) {
        wrapped.setTitle(value);
    }

    @Override
    public final void setInitialDirectory(File value) {
        wrapped.setInitialDirectory(value);
    }

    public File showDialog(Window ownerWindow) {
        File selected = wrapped.showDialog(ownerWindow);
        if (selected != null && selected.isDirectory()) {
            eventStudio().broadcast(new SetLatestDirectoryEvent(selected));
        }
        return selected;
    }

}
