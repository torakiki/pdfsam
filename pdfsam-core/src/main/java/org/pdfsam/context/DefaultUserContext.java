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

import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link Preferences} implementation for the {@link UserContext}.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class DefaultUserContext implements UserContext {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultUserContext.class);

    static final String CHECK_FOR_UPDATES_PROP = "org.pdfsam.default.checkforupdate";
    static final String CHECK_FOR_NEWS_PROP = "org.pdfsam.default.checkfornews";
    static final String DONATE_NOTIFICATION_PROP = "org.pdfsam.default.donate.notification";
    static final String PLAY_SOUNDS_PROP = "org.pdfsam.default.play.sounds";
    static final String FETCH_PREMIUM_MODULES_PROP = "org.pdfsam.default.fetch.premium.modules";
    static final String LOCALE_PROP = "org.pdfsam.default.locale";

    private Preferences prefs;

    public DefaultUserContext() {
        initNode();
    }

    private void initNode() {
        this.prefs = Preferences.userRoot().node("/org/pdfsam/user/conf");
    }

    @Override
    public String getDefaultWorkspacePath() {
        return prefs.get(StringUserPreference.WORKSPACE_PATH.toString(), StringUtils.EMPTY);
    }

    @Override
    public String getDefaultWorkingPath() {
        return prefs.get(StringUserPreference.WORKING_PATH.toString(), StringUtils.EMPTY);
    }

    @Override
    public boolean isPlaySounds() {
        return prefs.getBoolean(BooleanUserPreference.PLAY_SOUNDS.toString(),
                Boolean.valueOf(System.getProperty(PLAY_SOUNDS_PROP, Boolean.TRUE.toString())));
    }

    @Override
    public boolean isDonationNotification() {
        return prefs.getBoolean(BooleanUserPreference.DONATION_NOTIFICATION.toString(),
                Boolean.valueOf(System.getProperty(DONATE_NOTIFICATION_PROP, Boolean.TRUE.toString())));
    }

    @Override
    public boolean isFetchPremiumModules() {
        return prefs.getBoolean(BooleanUserPreference.PREMIUM_MODULES.toString(),
                Boolean.valueOf(System.getProperty(FETCH_PREMIUM_MODULES_PROP, Boolean.TRUE.toString())));
    }

    @Override
    public boolean isUseSmartOutput() {
        return prefs.getBoolean(BooleanUserPreference.SMART_OUTPUT.toString(), Boolean.TRUE);
    }

    @Override
    public boolean isSaveWorkspaceOnExit() {
        return prefs.getBoolean(BooleanUserPreference.SAVE_WORKSPACE_ON_EXIT.toString(), Boolean.FALSE);
    }

    @Override
    public int getNumberOfLogRows() {
        return prefs.getInt(IntUserPreference.LOGVIEW_ROWS_NUMBER.toString(), 200);
    }

    @Override
    public String getStartupModule() {
        return prefs.get(StringUserPreference.STARTUP_MODULE.toString(), StringUtils.EMPTY);
    }

    @Override
    public boolean isCheckForUpdates() {
        return prefs.getBoolean(BooleanUserPreference.CHECK_UPDATES.toString(),
                Boolean.valueOf(System.getProperty(CHECK_FOR_UPDATES_PROP, Boolean.TRUE.toString())));
    }

    @Override
    public boolean isCheckForNews() {
        return prefs.getBoolean(BooleanUserPreference.CHECK_FOR_NEWS.toString(),
                Boolean.valueOf(System.getProperty(CHECK_FOR_NEWS_PROP, Boolean.TRUE.toString())));
    }

    @Override
    public boolean isCompressionEnabled() {
        return prefs.getBoolean(BooleanUserPreference.PDF_COMPRESSION_ENABLED.toString(), Boolean.TRUE);
    }

    @Override
    public boolean isSavePwdInWorkspaceFile() {
        return prefs.getBoolean(BooleanUserPreference.SAVE_PWD_IN_WORKSPACE.toString(), Boolean.FALSE);
    }

    @Override
    public String getLocale() {
        return prefs.get(StringUserPreference.LOCALE.toString(), System.getProperty(LOCALE_PROP));
    }

    @Override
    public void clear() {
        try {
            prefs.removeNode();
            prefs.flush();
            initNode();
        } catch (BackingStoreException e) {
            LOG.error("Unable to clear user preferences", e);
        }
    }

    @Override
    public void setBooleanPreference(BooleanUserPreference pref, boolean value) {
        prefs.putBoolean(pref.toString(), value);
    }

    @Override
    public void setIntegerPreference(IntUserPreference pref, int value) {
        prefs.putInt(pref.toString(), value);
    }

    @Override
    public void setStringPreference(StringUserPreference pref, String value) {
        prefs.put(pref.toString(), value);
    }
}
