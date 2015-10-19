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

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.pdfsam.ui.NewsPolicy;
import org.pdfsam.ui.Theme;

/**
 * Test unit for the {@link DefaultUserContext}.
 * 
 * @author Andrea Vacondio
 * 
 */
public class DefaultUserContextTest {

    private DefaultUserContext victim = new DefaultUserContext();

    @Before
    public void setUp() {
        victim.clear();
    }

    @After
    public void tearDown() {
        victim.clear();
    }

    @Test
    public void isCheckUpdates() {
        victim.setBooleanPreference(BooleanUserPreference.CHECK_UPDATES, false);
        assertFalse(victim.isCheckForUpdates());
        victim.setBooleanPreference(BooleanUserPreference.CHECK_UPDATES, true);
        assertTrue(victim.isCheckForUpdates());
    }

    @Test
    public void isCheckUpdatesSystemDefault() {
        System.setProperty(DefaultUserContext.CHECK_FOR_UPDATES_PROP, "false");
        assertFalse(victim.isCheckForUpdates());
        victim.setBooleanPreference(BooleanUserPreference.CHECK_UPDATES, true);
        assertTrue(victim.isCheckForUpdates());
        System.clearProperty(DefaultUserContext.CHECK_FOR_UPDATES_PROP);

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
    public void clear() {
        assertTrue(victim.isUseSmartOutput());
        victim.setBooleanPreference(BooleanUserPreference.SMART_OUTPUT, false);
        assertFalse(victim.isUseSmartOutput());
        victim.clear();
        assertTrue(victim.isUseSmartOutput());
    }

    @Test
    public void getLocale() {
        victim.setStringPreference(StringUserPreference.LOCALE, "en-GB");
        assertEquals("en-GB", victim.getLocale());
        victim.setStringPreference(StringUserPreference.LOCALE, "");
        assertTrue(isBlank(victim.getLocale()));
    }

    @Test
    public void getLocalesystemProp() {
        System.setProperty(DefaultUserContext.LOCALE_PROP, "es");
        assertEquals("es", victim.getLocale());
        victim.setStringPreference(StringUserPreference.LOCALE, "");
        assertTrue(isBlank(victim.getLocale()));
        System.clearProperty(DefaultUserContext.LOCALE_PROP);
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
        assertEquals(Theme.ROUNDISH.toString(), victim.getTheme());
    }

    @Test
    public void getStartupModule() {
        victim.setStringPreference(StringUserPreference.STARTUP_MODULE, "ChuckNorris");
        assertEquals("ChuckNorris", victim.getStartupModule());
        victim.setStringPreference(StringUserPreference.STARTUP_MODULE, "");
        assertTrue(isBlank(victim.getStartupModule()));
    }

    @Test
    public void getNewsPolicy() {
        victim.setStringPreference(StringUserPreference.NEWS_POLICY, NewsPolicy.ALWAYS.toString());
        assertEquals(NewsPolicy.ALWAYS, victim.getNewsPolicy());
        victim.setStringPreference(StringUserPreference.NEWS_POLICY, "");
        assertEquals(NewsPolicy.ONCE_A_WEEK, victim.getNewsPolicy());
    }

    @Test
    public void getNewsPolicySysPro() {
        System.setProperty(DefaultUserContext.NEWS_PROP, NewsPolicy.ONCE_A_DAY.toString());
        assertEquals(NewsPolicy.ONCE_A_DAY, victim.getNewsPolicy());
        victim.setStringPreference(StringUserPreference.NEWS_POLICY, "");
        assertEquals(NewsPolicy.ONCE_A_WEEK, victim.getNewsPolicy());
        System.clearProperty(DefaultUserContext.NEWS_PROP);
    }

    @Test
    public void getNewsPolicyFallback() {
        victim.setStringPreference(StringUserPreference.NEWS_POLICY, "ChuckNorris");
        assertEquals(NewsPolicy.ONCE_A_WEEK, victim.getNewsPolicy());
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

    @Test
    public void getNumberOfLogRows() {
        assertEquals(200, victim.getNumberOfLogRows());
        victim.setIntegerPreference(IntUserPreference.LOGVIEW_ROWS_NUMBER, 20);
        assertEquals(20, victim.getNumberOfLogRows());
    }
}
