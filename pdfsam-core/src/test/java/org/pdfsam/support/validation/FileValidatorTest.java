/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/ott/2013
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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 *
 */
public class FileValidatorTest {
    private Validator<String> victim = Validators.existingFile();

    @Test
    public void testNegative() {
        assertFalse(victim.isValid("/Chuck/Norris"));
    }

    @Test
    public void testPositive() throws IOException {
        Path test = Files.createTempFile("tmp", "test");
        assertTrue(victim.isValid(test.toAbsolutePath().toString()));
        Files.delete(test);
    }

    @Test
    public void testAllowBlank() {
        assertFalse(victim.isValid(""));
        assertTrue(Validators.validEmpty(victim).isValid(""));
    }

}
