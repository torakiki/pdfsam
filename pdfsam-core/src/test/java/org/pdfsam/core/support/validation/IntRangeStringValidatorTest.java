/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/nov/2012
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
package org.pdfsam.core.support.validation;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
public class IntRangeStringValidatorTest {
    private final Validator<String> victim = Validators.positiveIntRange(1, 5);

    @Test
    public void negative() {
        assertFalse(victim.isValid("-123"));
    }

    @Test
    public void invalid() {
        assertFalse(victim.isValid("dsdsa"));
    }

    @Test
    public void invalidLower() {
        assertThrows(IllegalArgumentException.class, () -> Validators.positiveIntRange(0, 5));
    }

    @Test
    public void invalidUpper() {
        assertThrows(IllegalArgumentException.class, () -> Validators.positiveIntRange(1, 0));
    }

    @Test
    public void valid() {
        assertTrue(victim.isValid("1"));
        assertTrue(victim.isValid("3"));
        assertTrue(victim.isValid("5"));
    }

    @Test
    public void allowBlank() {
        assertFalse(victim.isValid(""));
        assertTrue(Validators.validEmpty(victim).isValid(""));
    }
}
