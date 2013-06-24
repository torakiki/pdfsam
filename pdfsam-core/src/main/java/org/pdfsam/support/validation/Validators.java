/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.support.validation;

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
    public static Validator<String> newIntRangeString(int upper, int lower) {
        return new IntRangeStringValidator(upper, lower);
    }

}
