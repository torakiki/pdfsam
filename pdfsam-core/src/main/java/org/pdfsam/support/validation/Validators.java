/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import org.pdfsam.support.io.FileType;

/**
 * Provides Factory methods to create validators
 * 
 * @author Andrea Vacondio
 * 
 */
public final class Validators {

    private Validators() {
        // hide
    }

    /**
     * @return a new instance of a validator checking for a on blank input stringg
     */
    public static Validator<String> newNonBlankString() {
        return new NonBlankStringValidator();
    }

    /**
     * @return a new instance of a validator checking for a input string representing an integer number
     */
    public static Validator<String> newIntegerString() {
        return new IntegerStringValidator();
    }

    /**
     * @return a new instance of a validator checking for a input string representing an integer number in the given range
     */
    public static Validator<String> newIntRangeString(int lower, int upper) {
        return new IntRangeStringValidator(lower, upper);
    }

    /**
     * @return a new instance of a validator checking for a input string representing an existing file
     */
    public static Validator<String> newExistingFileString() {
        return new FileValidator();
    }

    /**
     * @return a new instance of a validator checking for a input string representing an existing file of the given type
     */
    public static Validator<String> newFileTypeString(FileType type) {
        return new FileTypeValidator(type);
    }

    /**
     * @return a new instance of a validator checking for a input string representing an existing directory
     */
    public static Validator<String> newExistingDirectoryString() {
        return new DirectoryValidator();
    }

    /**
     * @param decorate
     * @return a new instance of the a validator that considers blank string as valid, it delegates otherwise
     */
    public static Validator<String> decorateAsValidBlankString(Validator<String> decorate) {
        return new ValidBlankStringDecorator(decorate);
    }

    /**
     * Decorates the input validator handling blank strings as valid
     * 
     * @author Andrea Vacondio
     * 
     */
    static final class ValidBlankStringDecorator implements Validator<String> {
        private Validator<String> decorate;

        private ValidBlankStringDecorator(Validator<String> decorate) {
            this.decorate = decorate;
        }

        @Override
        public boolean isValid(String input) {
            if (isNotBlank(input)) {
                return decorate.isValid(input);
            }
            return true;
        }

    }
}
