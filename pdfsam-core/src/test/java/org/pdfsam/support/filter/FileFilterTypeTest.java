/*
 * Created on 15/dic/2011
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

    @Before
    public void setUp() {
        failing = mock(File.class);
        when(failing.getAbsolutePath()).thenReturn("/path/chuck.norris");
        when(failing.isDirectory()).thenReturn(Boolean.FALSE);
    }

    @Test
    public void testCsv() {
        File file = mock(File.class);
        when(file.getAbsolutePath()).thenReturn("/path/chuck.csv");
        assertTrue(FileFilterType.CSV.accept(file));
        assertFalse(FileFilterType.CSV.accept(failing));
    }

    @Test
    public void testJar() {
        File file = mock(File.class);
        when(file.getAbsolutePath()).thenReturn("/path/chuck.jar");
        assertTrue(FileFilterType.JAR.accept(file));
        assertFalse(FileFilterType.JAR.accept(failing));
    }

    @Test
    public void testTxt() {
        File file = mock(File.class);
        when(file.getAbsolutePath()).thenReturn("/path/chuck.txt");
        assertTrue(FileFilterType.TXT.accept(file));
        assertFalse(FileFilterType.TXT.accept(failing));
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
    }
}
