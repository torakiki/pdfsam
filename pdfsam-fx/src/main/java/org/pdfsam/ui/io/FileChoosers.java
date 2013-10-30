/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.io;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;

import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.support.io.FileType;

/**
 * Contains a single instance of {@link FileChooser} and provides static methods to get and configure the instance
 * 
 * @author Andrea Vacondio
 * 
 */
public final class FileChoosers {

    private static final FileChooser FILE_INSTANCE = new FileChooser();
    private static final DirectoryChooser DIR_INSTANCE = new DirectoryChooser();
    static {
        String defaultworkingPath = DefaultUserContext.getInstance().getDefaultWorkingPath();
        if (isNotBlank(defaultworkingPath)) {
            Path initialDir = Paths.get(defaultworkingPath);
            if (Files.isDirectory(initialDir)) {
                File file = new File(DefaultUserContext.getInstance().getDefaultWorkingPath());
                FILE_INSTANCE.setInitialDirectory(file);
                DIR_INSTANCE.setInitialDirectory(file);
            }
        }
    }

    private FileChoosers() {
        // hide
    }

    /**
     * @param filter
     *            the file type extension accepted
     * @param title
     * @return a shared instance of {@link FileChooser} with the given title.
     */
    public static FileChooser getFileChooser(FileType filter, String title) {
        FILE_INSTANCE.getExtensionFilters().setAll(filter.getFilter());
        FILE_INSTANCE.setInitialFileName("");
        FILE_INSTANCE.setTitle(title);
        return FILE_INSTANCE;
    }

    /**
     * @param title
     * @return a shared instance of the {@link DirectoryChooser} with the given title.
     */
    public static DirectoryChooser getDirectoryChooser(String title) {
        DIR_INSTANCE.setTitle(title);
        return DIR_INSTANCE;
    }
}
