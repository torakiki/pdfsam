/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2011
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
package org.pdfsam.context;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

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
    public void isCheckForNews() {
        victim.setBooleanPreference(BooleanUserPreference.CHECK_FOR_NEWS, false);
        assertFalse(victim.isCheckForNews());
        victim.setBooleanPreference(BooleanUserPreference.CHECK_FOR_NEWS, true);
        assertTrue(victim.isCheckForNews());
    }

    @Test
    public void isCheckNewsSystemDefault() {
        System.setProperty(DefaultUserContext.CHECK_FOR_NEWS_PROP, "false");
        assertFalse(victim.isCheckForNews());
        victim.setBooleanPreference(BooleanUserPreference.CHECK_FOR_NEWS, true);
        assertTrue(victim.isCheckForNews());
        System.clearProperty(DefaultUserContext.CHECK_FOR_NEWS_PROP);
    }

    @Test
    public void isSaveWokspaceOnExit() {
        victim.setBooleanPreference(BooleanUserPreference.SAVE_WORKSPACE_ON_EXIT, false);
        assertFalse(victim.isSaveWorkspaceOnExit());
        victim.setBooleanPreference(BooleanUserPreference.SAVE_WORKSPACE_ON_EXIT, true);
        assertTrue(victim.isSaveWorkspaceOnExit());
    }

    @Test
    public void isSavePwdInWorkspace() {
        victim.setBooleanPreference(BooleanUserPreference.SAVE_PWD_IN_WORKSPACE, false);
        assertFalse(victim.isSavePwdInWorkspaceFile());
        victim.setBooleanPreference(BooleanUserPreference.SAVE_PWD_IN_WORKSPACE, true);
        assertTrue(victim.isSavePwdInWorkspaceFile());
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
    public void isPlaySoundsSystemDefault() {
        System.setProperty(DefaultUserContext.PLAY_SOUNDS_PROP, "false");
        assertFalse(victim.isPlaySounds());
        victim.setBooleanPreference(BooleanUserPreference.PLAY_SOUNDS, true);
        assertTrue(victim.isPlaySounds());
        System.clearProperty(DefaultUserContext.PLAY_SOUNDS_PROP);
    }

    @Test
    public void isDonationNotification() {
        victim.setBooleanPreference(BooleanUserPreference.DONATION_NOTIFICATION, false);
        assertFalse(victim.isDonationNotification());
        victim.setBooleanPreference(BooleanUserPreference.DONATION_NOTIFICATION, true);
        assertTrue(victim.isDonationNotification());
    }

    @Test
    public void isDonationNotificationSystemDefault() {
        System.setProperty(DefaultUserContext.DONATE_NOTIFICATION_PROP, "false");
        assertFalse(victim.isDonationNotification());
        victim.setBooleanPreference(BooleanUserPreference.DONATION_NOTIFICATION, true);
        assertTrue(victim.isDonationNotification());
        System.clearProperty(DefaultUserContext.DONATE_NOTIFICATION_PROP);
    }

    @Test
    public void isPremiumModules() {
        victim.setBooleanPreference(BooleanUserPreference.PREMIUM_MODULES, false);
        assertFalse(victim.isFetchPremiumModules());
        victim.setBooleanPreference(BooleanUserPreference.PREMIUM_MODULES, true);
        assertTrue(victim.isFetchPremiumModules());
    }

    @Test
    public void isPremiumModulesSystemDefault() {
        System.setProperty(DefaultUserContext.FETCH_PREMIUM_MODULES_PROP, "false");
        assertFalse(victim.isFetchPremiumModules());
        victim.setBooleanPreference(BooleanUserPreference.PREMIUM_MODULES, true);
        assertTrue(victim.isFetchPremiumModules());
        System.clearProperty(DefaultUserContext.FETCH_PREMIUM_MODULES_PROP);
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
    public void getStartupModule() {
        victim.setStringPreference(StringUserPreference.STARTUP_MODULE, "ChuckNorris");
        assertEquals("ChuckNorris", victim.getStartupModule());
        victim.setStringPreference(StringUserPreference.STARTUP_MODULE, "");
        assertTrue(isBlank(victim.getStartupModule()));
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
    public void getNumberOfLogRows() {
        assertEquals(200, victim.getNumberOfLogRows());
        victim.setIntegerPreference(IntUserPreference.LOGVIEW_ROWS_NUMBER, 20);
        assertEquals(20, victim.getNumberOfLogRows());
    }
}
