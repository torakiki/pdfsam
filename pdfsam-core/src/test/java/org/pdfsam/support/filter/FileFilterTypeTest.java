/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/dic/2011
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
public class FileFilterTypeTest {

    private File failing;
    private File directory;

    @Before
    public void setUp() {
        failing = mock(File.class);
        when(failing.getAbsolutePath()).thenReturn("/path/chuck.norris");
        when(failing.isDirectory()).thenReturn(Boolean.FALSE);
        directory = mock(File.class);
        when(directory.getAbsolutePath()).thenReturn("/path/chuck");
        when(directory.isDirectory()).thenReturn(Boolean.TRUE);
    }

    @Test
    public void testCsv() {
        File file = mock(File.class);
        when(file.getAbsolutePath()).thenReturn("/path/chuck.csv");
        assertTrue(FileFilterType.CSV.accept(file));
        assertFalse(FileFilterType.CSV.accept(failing));
        assertFalse(FileFilterType.CSV.accept(directory));
    }

    @Test
    public void testJar() {
        File file = mock(File.class);
        when(file.getAbsolutePath()).thenReturn("/path/chuck.jar");
        assertTrue(FileFilterType.JAR.accept(file));
        assertFalse(FileFilterType.JAR.accept(failing));
        assertFalse(FileFilterType.JAR.accept(directory));
    }

    @Test
    public void testTxt() {
        File file = mock(File.class);
        when(file.getAbsolutePath()).thenReturn("/path/chuck.txt");
        assertTrue(FileFilterType.TXT.accept(file));
        assertFalse(FileFilterType.TXT.accept(failing));
        assertFalse(FileFilterType.TXT.accept(directory));
    }

    @Test
    public void testXml() {
        File file = mock(File.class);
        when(file.getAbsolutePath()).thenReturn("/path/chuck.xml");
        assertTrue(FileFilterType.XML.accept(file));
        assertFalse(FileFilterType.XML.accept(failing));
    }

    @Test
    public void testPdf() {
        File file = mock(File.class);
        when(file.getAbsolutePath()).thenReturn("/path/chuck.pdf");
        assertTrue(FileFilterType.PDF.accept(file));
        assertFalse(FileFilterType.PDF.accept(failing));
        assertFalse(FileFilterType.PDF.accept(directory));
    }

    @Test
    public void testHtml() {
        File file = mock(File.class);
        when(file.getAbsolutePath()).thenReturn("/path/chuck.html");
        assertTrue(FileFilterType.HTML.accept(file));
        when(file.getAbsolutePath()).thenReturn("/path/chuck.htm");
        assertTrue(FileFilterType.HTML.accept(file));
        assertFalse(FileFilterType.HTML.accept(failing));
    }

    @Test
    public void testDirs() {
        File file = mock(File.class);
        when(file.isDirectory()).thenReturn(Boolean.TRUE);
        assertTrue(FileFilterType.DIRECTORIES.accept(file));
        assertFalse(FileFilterType.DIRECTORIES.accept(failing));
        assertTrue(FileFilterType.DIRECTORIES.accept(directory));
    }
}
