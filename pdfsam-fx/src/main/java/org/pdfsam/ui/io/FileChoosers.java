/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/ott/2013
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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

import org.pdfsam.support.io.FileType;

/**
 * Contains a single instance of {@link javafx.stage.FileChooser} and provides static methods to get and configure the instance
 * 
 * @author Andrea Vacondio
 * 
 */
public final class FileChoosers {
    private static final RememberingLatestFileChooserWrapper FILE_INSTANCE = new RememberingLatestFileChooserWrapper();
    private static final RememberingLatestDirectoryChooserWrapper DIR_INSTANCE = new RememberingLatestDirectoryChooserWrapper();

    private FileChoosers() {
        // hide
    }

    /**
     * @param filter
     *            the file type extension accepted
     * @param title
     * @return a shared instance of {@link javafx.stage.FileChooser} with the given title.
     */
    public static RememberingLatestFileChooserWrapper getFileChooser(FileType filter, String title) {
        FILE_INSTANCE.getExtensionFilters().setAll(filter.getFilter());
        FILE_INSTANCE.setInitialFileName("");
        FILE_INSTANCE.setTitle(title);
        return FILE_INSTANCE;
    }

    /**
     * @param title
     * @return a shared instance of the {@link javafx.stage.DirectoryChooser} with the given title.
     */
    public static RememberingLatestDirectoryChooserWrapper getDirectoryChooser(String title) {
        DIR_INSTANCE.setTitle(title);
        return DIR_INSTANCE;
    }
}
