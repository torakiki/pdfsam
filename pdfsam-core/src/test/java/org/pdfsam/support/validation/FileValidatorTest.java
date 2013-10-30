/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.Assert;
import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class FileValidatorTest {
    private Validator<String> victim = Validators.newExistingFileString();

    @Test
    public void testNegative() {
        Assert.assertFalse(victim.isValid("/Chuck/Norris"));
    }

    @Test
    public void testPositive() throws IOException {
        Path test = Files.createTempFile("tmp", "test");
        Assert.assertTrue(victim.isValid(test.toAbsolutePath().toString()));
        Files.delete(test);
    }

    @Test
    public void testAllowBlank() {
        Assert.assertTrue(victim.isValid(""));
    }

}
