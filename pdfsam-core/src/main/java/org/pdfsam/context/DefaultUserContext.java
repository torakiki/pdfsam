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

import static org.apache.commons.lang3.StringUtils.defaultIfBlank;

import java.util.prefs.Preferences;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.ui.NewsPolicy;
import org.pdfsam.ui.Theme;

/**
 * {@link Preferences} implementation for the {@link UserContext}.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class DefaultUserContext implements UserContext {

    private Preferences prefs;
    private UserWorkspacesContext workspaces;

    public DefaultUserContext() {
        this.prefs = Preferences.userRoot().node("/org/pdfsam/user/conf");
        this.workspaces = new PreferencesUserWorkspacesContext();
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
        return prefs.getBoolean(BooleanUserPreference.PLAY_SOUNDS.toString(), Boolean.TRUE);
    }

    @Override
    public boolean isUseSmartOutput() {
        return prefs.getBoolean(BooleanUserPreference.SMART_OUTPUT.toString(), Boolean.TRUE);
    }

    @Override
    public int getThumbnailsSize() {
        return prefs.getInt(IntUserPreference.THUMBNAILS_SIZE.toString(), 190);
    }

    @Override
    public String getTheme() {
        return defaultIfBlank(prefs.get(StringUserPreference.THEME.toString(), StringUtils.EMPTY),
                Theme.ROUNDISH.toString());
    }

    @Override
    public String getStartupModule() {
        return prefs.get(StringUserPreference.STARTUP_MODULE.toString(), StringUtils.EMPTY);
    }

    @Override
    public String getNewsPolicy() {
        return defaultIfBlank(prefs.get(StringUserPreference.NEWS_POLICY.toString(), StringUtils.EMPTY),
                NewsPolicy.ONCE_A_WEEK.toString());
    }

    @Override
    public String getThumbnailsCreatorIdentifier() {
        // TODO identifier
        return prefs.get(StringUserPreference.THUMBNAILS_IDENTIFIER.toString(), StringUtils.EMPTY);
    }

    @Override
    public boolean isCheckForUpdates() {
        return prefs.getBoolean(BooleanUserPreference.CHECK_UPDATES.toString(), Boolean.TRUE);
    }

    @Override
    public boolean isHighQualityThumbnails() {
        return prefs.getBoolean(BooleanUserPreference.HIGH_QUALITY_THUMB.toString(), Boolean.FALSE);
    }

    @Override
    public String getLocale() {
        return prefs.get(StringUserPreference.LOCALE.toString(), StringUtils.EMPTY);
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

    @Override
    public UserWorkspacesContext getUserWorkspacesContext() {
        return workspaces;
    }

}
