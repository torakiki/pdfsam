/*
 * Created on 15/dic/2011
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.support.filter;

import java.io.File;

import org.apache.commons.io.FilenameUtils;
import org.pdfsam.context.DefaultI18nContext;

/**
 * @author Andrea Vacondio
 * 
 */
public enum FileFilterType {
    CSV("(*.csv)", "csv"),
    JAR("(*.jar)", "jar"),
    PDF("(*.pdf)", "pdf"),
    TXT("(*.txt)", "txt"),
    LOG("(*.log)", "log"),
    XML("(*.xml)", "xml"),
    HTML("(*.html, *.htm)", "html", "htm"),
    DIRECTORIES(DefaultI18nContext.getInstance().getI18n().tr("Directories"));

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
            if (acceptedExtensions != null) {
                for (String accepted : acceptedExtensions) {
                    if (accepted.equalsIgnoreCase(FilenameUtils.getExtension(file.getAbsolutePath()))) {
                        return true;
                    }
                }
            }
            return file.isDirectory();
        }
        return false;
    }

}
