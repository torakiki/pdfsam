/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.pdfsam.support.validation.Validators.and;
import static org.pdfsam.support.validation.Validators.not;

import org.junit.Test;

public class ValidatorsTest {

    @Test
    public void negateValidator() {
        Validator<String> victim = Validators.nonBlank();
        assertTrue(victim.isValid("dads"));
        assertFalse(victim.isValid(" "));
        assertFalse(not(victim).isValid("dads"));
        assertTrue(not(victim).isValid(" "));
    }

    @Test
    public void andValidator() {
        Validator<String> first = Validators.nonBlank();
        Validator<String> second = Validators.positiveInteger();
        assertTrue(first.isValid("dads"));
        assertFalse(first.isValid(" "));
        assertTrue(second.isValid("10"));
        assertFalse(second.isValid("-10"));
        assertFalse(and(first, second).isValid(" "));
        assertFalse(and(first, second).isValid("dads"));
        assertTrue(and(first, second).isValid("10"));
        assertFalse(and(first, second).isValid("-10"));
    }
}
