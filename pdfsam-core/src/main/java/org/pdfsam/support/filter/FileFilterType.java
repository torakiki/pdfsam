/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/dic/2011
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
package org.pdfsam.support.filter;

import java.io.File;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.validation.Validator;

import static org.apache.commons.lang3.ArrayUtils.isNotEmpty;

/**
 * Types of filters that can be used in file chooser
 * 
 * @author Andrea Vacondio
 * 
 */
public enum FileFilterType implements Validator<String> {
    CSV("(*.csv)", "csv"),
    JAR("(*.jar)", "jar"),
    PDF("(*.pdf)", "pdf"),
    TXT("(*.txt)", "txt"),
    LOG("(*.log)", "log"),
    XML("(*.xml)", "xml"),
    HTML("(*.html, *.htm)", "html", "htm"),
    DIRECTORIES(DefaultI18nContext.getInstance().i18n("Directories"));

    private String description;
    private String[] acceptedExtensions;

    private FileFilterType(String description, String... extensions) {
        this.description = description;
        this.acceptedExtensions = extensions;
    }

    String getDescription() {
        return description;
    }

    boolean accept(File file) {
        if (file != null) {
            if (isNotEmpty(acceptedExtensions)) {
                return isAccepted(file);
            }
            return file.isDirectory();
        }
        return false;
    }

    private boolean isAccepted(File file) {
        for (String accepted : acceptedExtensions) {
            if (accepted.equalsIgnoreCase(FilenameUtils.getExtension(file.getAbsolutePath()))) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean isValid(String input) {
        try {
            return StringUtils.isNotBlank(input) && accept(Paths.get(input).toFile());
        } catch (InvalidPathException ipe) {
            return false;
        }
    }

}
