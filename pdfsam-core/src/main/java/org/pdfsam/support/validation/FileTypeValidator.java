/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 27/ott/2013
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
package org.pdfsam.support.validation;

import static org.pdfsam.support.RequireUtils.requireNotNull;

import org.pdfsam.support.io.FileType;

/**
 * Validates that a given file path is existing and of the expected type
 * 
 * @author Andrea Vacondio
 * 
 */
class FileTypeValidator extends FileValidator {

    private FileType type;
    private boolean mustExist = true;

    public FileTypeValidator(FileType type, boolean mustExist) {
        requireNotNull(type, "FileType cannot be null");
        this.type = type;
        this.mustExist = mustExist;
    }

    @Override
    public boolean isValid(String input) {
        return (!mustExist || super.isValid(input)) && type.matches(input);
    }
}
