/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/ago/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.configuration;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * @author Andrea Vacondio
 *
 */
public class JarSignatureFilterTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private JarSignatureFilter victim = new JarSignatureFilter();

    @Test
    public void isNotJar() throws IOException {
        assertFalse(victim.test(folder.newFile("test.txt").toPath()));
    }

    @Test
    public void isNull() {
        assertFalse(victim.test(null));
    }

    @Test
    public void isCaseInsensitive() throws IOException {
        assertTrue(victim.test(folder.newFile("test.jar").toPath()));
        assertTrue(victim.test(folder.newFile("test.JaR").toPath()));
        assertTrue(victim.test(folder.newFile("test.JAR").toPath()));
    }

}
