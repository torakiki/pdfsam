/*
 * This file is part of the PDF Split And Merge source code
 * Created on 18/ott/2013
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
package org.pdfsam.model.io;

import javafx.stage.FileChooser.ExtensionFilter;
import org.apache.commons.io.IOCase;

import static org.apache.commons.io.FilenameUtils.wildcardMatch;

/**
 * Types of file handled by PDFsam
 *
 * @author Andrea Vacondio
 */
public enum FileType {
    ALL("(*.*)", "*.*", "*"),
    CSV("(*.csv)", "*.csv", "*.CSV"),
    PDF("(*.pdf)", "*.pdf", "*.PDF"),
    TXT("(*.txt)", "*.txt", "*.TXT"),
    LOG("(*.log, *.txt)", "*.log", "*.txt", "*.LOG", "*.TXT"),
    XML("(*.xml)", "*.xml", "*.XML"),
    JSON("(*.json)", "*.json", "*.JSON"),
    HTML("(*.html, *.htm)", "*.htm", "*.html", "*.HTM", "*.HTML");
    private final ExtensionFilter filter;

    FileType(String description, String... extensions) {
        this.filter = new ExtensionFilter(description, extensions);
    }

    public ExtensionFilter getFilter() {
        return filter;
    }

    /**
     * @param filename
     * @return true if the input filename is of this {@link FileType}
     */
    public boolean matches(String filename) {
        return getFilter().getExtensions().stream().anyMatch(ext -> wildcardMatch(filename, ext, IOCase.INSENSITIVE));

    }
}
