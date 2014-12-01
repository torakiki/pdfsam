/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ago/2014
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
package org.pdfsam.update;

import static org.junit.Assert.assertEquals;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeJavaFxThreadRule;

/**
 * @author Andrea Vacondio
 *
 */
public class DefaultUpdateServiceTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    @Rule
    public InitializeJavaFxThreadRule javaFX = new InitializeJavaFxThreadRule();

    @Test
    public void pasitiveCheckForUpdates() {
        DefaultUpdateService victim = new DefaultUpdateService("{\"currentVersion\" : \"3.0.0\"}");
        assertEquals("3.0.0", victim.getLatestVersion());
    }

    @Test
    public void negativeCheckForUpdates() {
        DefaultUpdateService victim = new DefaultUpdateService("ChuckNorris");
        assertEquals("", victim.getLatestVersion());
    }
}
