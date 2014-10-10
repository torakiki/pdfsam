/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.context;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import org.junit.AfterClass;
import org.junit.Test;

/**
 * Test unit for the {@link DefaultUserContext}.
 * 
 * @author Andrea Vacondio
 * 
 */
public class DefaultUserContextTest {

    private DefaultUserContext victim = new DefaultUserContext();

    @AfterClass
    public static void tearDown() throws BackingStoreException {
        Preferences node = Preferences.userRoot().node("/pdfsam/user/conf");
        node.removeNode();
        node.flush();
    }

    @Test
    public void isCheckUpdates() {
        victim.setBooleanPreference(BooleanUserPreference.CHECK_UPDATES, false);
        assertFalse(victim.isCheckForUpdates());
        victim.setBooleanPreference(BooleanUserPreference.CHECK_UPDATES, true);
        assertTrue(victim.isCheckForUpdates());
    }

    @Test
    public void isUseSmartOutput() {
        victim.setBooleanPreference(BooleanUserPreference.SMART_OUTPUT, false);
        assertFalse(victim.isUseSmartOutput());
        victim.setBooleanPreference(BooleanUserPreference.SMART_OUTPUT, true);
        assertTrue(victim.isUseSmartOutput());
    }

    @Test
    public void isAskPlaySounds() {
        victim.setBooleanPreference(BooleanUserPreference.PLAY_SOUNDS, true);
        assertTrue(victim.isPlaySounds());
        victim.setBooleanPreference(BooleanUserPreference.PLAY_SOUNDS, false);
        assertFalse(victim.isPlaySounds());
    }

    @Test
    public void isHighQualityThumbnails() {
        victim.setBooleanPreference(BooleanUserPreference.HIGH_QUALITY_THUMB, false);
        assertFalse(victim.isHighQualityThumbnails());
        victim.setBooleanPreference(BooleanUserPreference.HIGH_QUALITY_THUMB, true);
        assertTrue(victim.isHighQualityThumbnails());

    }

    @Test
    public void getLocale() {
        victim.setStringPreference(StringUserPreference.LOCALE, "en-GB");
        assertEquals("en-GB", victim.getLocale());
        victim.setStringPreference(StringUserPreference.LOCALE, "");
        assertTrue(isBlank(victim.getLocale()));
    }

    @Test
    public void getThumbIdentifier() {
        victim.setStringPreference(StringUserPreference.THUMBNAILS_IDENTIFIER, "ChuckNorris");
        assertEquals("ChuckNorris", victim.getThumbnailsCreatorIdentifier());
        victim.setStringPreference(StringUserPreference.THUMBNAILS_IDENTIFIER, "");
        assertTrue(isBlank(victim.getThumbnailsCreatorIdentifier()));
    }

    @Test
    public void getTheme() {
        victim.setStringPreference(StringUserPreference.THEME, "ChuckNorris");
        assertEquals("ChuckNorris", victim.getTheme());
        victim.setStringPreference(StringUserPreference.THEME, "");
        assertTrue(isBlank(victim.getTheme()));
    }

    @Test
    public void getDefaultWorkingPath() {
        victim.setStringPreference(StringUserPreference.WORKING_PATH, "/path");
        assertEquals("/path", victim.getDefaultWorkingPath());
        victim.setStringPreference(StringUserPreference.WORKING_PATH, "");
        assertTrue(isBlank(victim.getDefaultWorkingPath()));
    }

    @Test
    public void getDefaultWorkspacePath() {
        victim.setStringPreference(StringUserPreference.WORKSPACE_PATH, "/wpath");
        assertEquals("/wpath", victim.getDefaultWorkspacePath());
        victim.setStringPreference(StringUserPreference.WORKSPACE_PATH, "");
        assertTrue(isBlank(victim.getDefaultWorkspacePath()));
    }

    @Test
    public void getThumbPoolSize() {
        victim.setIntegerPreference(IntUserPreference.THUMBNAILS_SIZE, 2);
        assertEquals(2, victim.getThumbnailsSize());
    }
}
