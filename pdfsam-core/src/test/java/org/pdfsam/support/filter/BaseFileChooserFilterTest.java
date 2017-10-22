/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/giu/2013
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
package org.pdfsam.support.filter;

import java.io.File;

import org.junit.Before;
import org.junit.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author Andrea Vacondio
 * 
 */
public class BaseFileChooserFilterTest {

    private File directory;

    @Before
    public void setUp() {
        directory = mock(File.class);
        when(directory.getAbsolutePath()).thenReturn("/path/chuck");
        when(directory.isDirectory()).thenReturn(Boolean.TRUE);
    }

    @Test
    public void testJar() {
        BaseFileChooserFilter victim = new BaseFileChooserFilter(FileFilterType.JAR);
        File file = mock(File.class);
        when(file.getAbsolutePath()).thenReturn("/path/chuck.jar");
        assertTrue(victim.accept(file));
        assertTrue(victim.accept(directory));
        assertFalse(victim.accept(null));
    }
}
