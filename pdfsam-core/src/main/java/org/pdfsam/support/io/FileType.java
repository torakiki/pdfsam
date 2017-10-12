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
package org.pdfsam.support.io;

import static org.apache.commons.io.FilenameUtils.wildcardMatch;
import javafx.stage.FileChooser.ExtensionFilter;

import org.apache.commons.io.IOCase;

/**
 * Types of file handled by PDFsam
 * 
 * @author Andrea Vacondio
 * 
 */
public enum FileType {
    ALL {
        @Override
        public ExtensionFilter getFilter() {
            return new ExtensionFilter("(*.*)", "*.*", "*");
        }
    },
    CSV {
        @Override
        public ExtensionFilter getFilter() {
            return new ExtensionFilter("(*.csv)", "*.csv");
        }
    },
    PDF {
        @Override
        public ExtensionFilter getFilter() {
            return new ExtensionFilter("(*.pdf)", "*.pdf");
        }
    },
    TXT {
        @Override
        public ExtensionFilter getFilter() {
            return new ExtensionFilter("(*.txt)", "*.txt");
        }
    },
    LOG {
        @Override
        public ExtensionFilter getFilter() {
            return new ExtensionFilter("(*.log, *.txt)", "*.log", "*.txt");
        }
    },
    XML {
        @Override
        public ExtensionFilter getFilter() {
            return new ExtensionFilter("(*.xml)", "*.xml");
        }
    },
    JSON {
        @Override
        public ExtensionFilter getFilter() {
            return new ExtensionFilter("(*.json)", "*.json");
        }
    },
    HTML {
        @Override
        public ExtensionFilter getFilter() {
            return new ExtensionFilter("(*.html, *.htm)", "*.htm", "*.html");
        }
    };
    public abstract ExtensionFilter getFilter();

    /**
     * @param filename
     * @return true if the input filename is of this {@link FileType}
     */
    public boolean matches(String filename) {
        for (String current : getFilter().getExtensions()) {
            if (wildcardMatch(filename, current, IOCase.INSENSITIVE)) {
                return true;
            }
        }
        return false;
    }
}
