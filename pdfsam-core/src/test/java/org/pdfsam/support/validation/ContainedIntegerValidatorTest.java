/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22 ott 2016
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

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashSet;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 *
 */
public class ContainedIntegerValidatorTest {
    private Validator<String> victim = Validators.containedInteger(new HashSet<>(Arrays.asList(-2, 5, 6)));

    @Test
    public void testNotContained() {
        assertFalse(victim.isValid("50"));
    }

    @Test
    public void testInvalid() {
        assertFalse(victim.isValid("dsdsa"));
    }

    @Test
    public void testValid() {
        assertTrue(victim.isValid("5"));

    }
}
