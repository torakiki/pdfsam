/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/ott/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.mockito.Mockito.mock;

import java.io.IOException;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * @author Andrea Vacondio
 *
 */
public class EnhancedClassloaderProviderTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test(expected = IllegalArgumentException.class)
    public void nullArg() {
        EnhancedClassloaderProvider.classLoader(null);
    }

    @Test
    public void notExisting() {
        System.setProperty(EnhancedClassloaderProvider.PDFSAM_MODULES_DIRECTORY, "/chuck/norris/roundhouse");
        ClassLoader classLoader = mock(ClassLoader.class);
        assertEquals(classLoader, EnhancedClassloaderProvider.classLoader(classLoader));
        System.setProperty(EnhancedClassloaderProvider.PDFSAM_MODULES_DIRECTORY, "");
    }

    @Test
    public void emptyFolder() throws IOException {
        System.setProperty(EnhancedClassloaderProvider.PDFSAM_MODULES_DIRECTORY, folder.newFolder().getAbsolutePath());
        ClassLoader classLoader = mock(ClassLoader.class);
        assertEquals(classLoader, EnhancedClassloaderProvider.classLoader(classLoader));
        System.setProperty(EnhancedClassloaderProvider.PDFSAM_MODULES_DIRECTORY, "");
    }

    @Test
    public void emptySysProperty() {
        System.setProperty(EnhancedClassloaderProvider.PDFSAM_MODULES_DIRECTORY, "");
        ClassLoader classLoader = mock(ClassLoader.class);
        assertEquals(classLoader, EnhancedClassloaderProvider.classLoader(classLoader));
    }

    @Test
    public void notEmptyFolder() throws IOException {
        folder.newFile("test.jar");
        System.setProperty(EnhancedClassloaderProvider.PDFSAM_MODULES_DIRECTORY, folder.getRoot().getAbsolutePath());
        ClassLoader classLoader = mock(ClassLoader.class);
        assertNotEquals(classLoader, EnhancedClassloaderProvider.classLoader(classLoader));
        System.setProperty(EnhancedClassloaderProvider.PDFSAM_MODULES_DIRECTORY, "");
    }

}
