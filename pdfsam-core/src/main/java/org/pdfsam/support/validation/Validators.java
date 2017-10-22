/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/nov/2012
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

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Set;

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
     * @return a new instance of a validator that always returns false
     */
    public static Validator<String> alwaysFalse() {
        return v -> false;
    }

    /**
     * @return a new instance of a validator checking for a on blank input string
     */
    public static Validator<String> nonBlank() {
        return v -> isNotBlank(v);
    }

    /**
     * @return a new instance of a validator checking for a input string representing a positive integer number
     */
    public static Validator<String> positiveInteger() {
        return new PositiveIntegerStringValidator();
    }

    /**
     * @return a new instance of a validator checking for a input string representing a positive integer number in the given range
     */
    public static Validator<String> positiveIntRange(int lower, int upper) {
        return new PositiveIntRangeStringValidator(lower, upper);
    }

    /**
     * @return a new instance of a validator checking for a input string representing an integer contained in the given set
     */
    public static Validator<String> containedInteger(Set<Integer> validValues) {
        return new ContainedIntegerValidator(validValues);
    }

    /**
     * @return a new instance of a validator checking for an input string representing an existing file. Blank string are invalid.
     * @see Validators#validEmpty(Validator)
     */
    public static Validator<String> existingFile() {
        return new FileValidator();
    }

    /**
     * @return a new instance of a validator checking for an input string representing an existing file of the given type. Blank string are invalid.
     * @param type
     *            type of the file represented by the input string
     * @see Validators#validEmpty(Validator)
     */
    public static Validator<String> existingFileType(FileType type) {
        return new FileTypeValidator(type, true);
    }

    /**
     * @return a new instance of a validator checking for an input string representing a file of the given type. Blank string are invalid.
     * @param type
     *            type of the file represented by the input string
     * @param mustExist
     *            if true the validator enforces an existing file
     * @see Validators#validEmpty(Validator)
     */
    public static Validator<String> fileType(FileType type, boolean mustExist) {
        return new FileTypeValidator(type, mustExist);
    }

    /**
     * @return a new instance of a validator checking for an input string representing an existing directory. Blank string are invalid.
     * @see Validators#validEmpty(Validator)
     */
    public static Validator<String> existingDirectory() {
        return v -> isNotBlank(v) && Files.isDirectory(Paths.get(v));
    }

    /**
     * @return a new instance of a validator checking for an input string matching the given regex.
     * @see Validators#validEmpty(Validator)
     */
    public static Validator<String> regexMatching(String regex) {
        return new RegexValidator(regex);
    }

    /**
     * @param validator
     * @return a new instance of the a validator that considers empty string as valid, it delegates otherwise
     */
    public static Validator<String> validEmpty(Validator<String> validator) {
        return v -> {
            if (isNotEmpty(v)) {
                return validator.isValid(v);
            }
            return true;
        };
    }

    /**
     * @param validator
     * @return a new instance of a validator that negates the given one
     */
    public static Validator<String> not(Validator<String> validator) {
        return v -> !validator.isValid(v);
    }

    /**
     * @param validator
     * @return a new instance of a validator that returns true if all the given validators return true
     */
    public static Validator<String> and(Validator<String>... validators) {
        return v -> Arrays.stream(validators).allMatch(validator -> validator.isValid(v));
    }
}
