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

import static org.apache.commons.lang3.StringUtils.isNotEmpty;

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
     * @return a new instance of a validator checking for a on blank input string
     */
    public static Validator<String> newNonBlankString() {
        return new NonBlankStringValidator();
    }

    /**
     * @return a new instance of a validator checking for a input string representing a positive integer number
     */
    public static Validator<String> newPositiveIntegerString() {
        return new PositiveIntegerStringValidator();
    }

    /**
     * @return a new instance of a validator checking for a input string representing an integer number in the given range
     */
    public static Validator<String> newIntRangeString(int lower, int upper) {
        return new IntRangeStringValidator(lower, upper);
    }

    /**
     * @return a new instance of a validator checking for an input string representing an existing file. Blank string are invalid.
     * @see Validators#decorateAsValidEmptyString(Validator)
     */
    public static Validator<String> newExistingFileString() {
        return new FileValidator();
    }

    /**
     * @return a new instance of a validator checking for an input string representing an existing file of the given type. Blank string are invalid.
     * @param type
     *            type of the file represented by the input string
     * @see Validators#decorateAsValidEmptyString(Validator)
     */
    public static Validator<String> newExistingFileTypeString(FileType type) {
        return new FileTypeValidator(type, true);
    }

    /**
     * @return a new instance of a validator checking for an input string representing a file of the given type. Blank string are invalid.
     * @param type
     *            type of the file represented by the input string
     * @param mustExist
     *            if true the validator enforces an existing file
     * @see Validators#decorateAsValidEmptyString(Validator)
     */
    public static Validator<String> newFileTypeString(FileType type, boolean mustExist) {
        return new FileTypeValidator(type, mustExist);
    }

    /**
     * @return a new instance of a validator checking for an input string representing an existing directory. Blank string are invalid.
     * @see Validators#decorateAsValidEmptyString(Validator)
     */
    public static Validator<String> newExistingDirectoryString() {
        return new DirectoryValidator();
    }

    /**
     * @return a new instance of a validator checking for an input string matching the given regex.
     * @see Validators#decorateAsValidEmptyString(Validator)
     */
    public static Validator<String> newRegexMatchingString(String regex) {
        return new RegexValidator(regex);
    }

    /**
     * @param decorate
     * @return a new instance of the a validator that considers empty string as valid, it delegates otherwise
     */
    public static Validator<String> decorateAsValidEmptyString(Validator<String> decorate) {
        return new ValidEmptyStringDecorator(decorate);
    }

    /**
     * Decorates the input validator handling empty strings as valid
     * 
     * @author Andrea Vacondio
     * 
     */
    static final class ValidEmptyStringDecorator implements Validator<String> {
        private Validator<String> decorate;

        private ValidEmptyStringDecorator(Validator<String> decorate) {
            this.decorate = decorate;
        }

        @Override
        public boolean isValid(String input) {
            if (isNotEmpty(input)) {
                return decorate.isValid(input);
            }
            return true;
        }

    }
}
