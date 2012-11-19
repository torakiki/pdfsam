/*
 * Created on 16/nov/2012
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
