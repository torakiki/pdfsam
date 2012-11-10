/*
 * Created on 12/dic/2011
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
package org.pdfsam.context;

import java.util.prefs.Preferences;

import org.apache.commons.lang3.StringUtils;

/**
 * {@link Preferences} implementation for the {@link UserContext}.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class DefaultUserContext implements UserContext {

    private Preferences prefs;
    private UserWorkspacesContext workspaces;

    private DefaultUserContext() {
        this.prefs = Preferences.userRoot().node("/pdfsam/user/conf");
        this.workspaces = new PreferencesUserWorkspacesContext();
    }

    /**
     * @return the default application context instance
     */
    public static UserContext getInstance() {
        return DefaultUserContextHolder.CONTEXT;
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
    public int getThumbnailsSize() {
        return prefs.getInt(IntUserPreference.THUMBNAILS_SIZE.toString(), 190);
    }

    @Override
    public String getLookAndFeelClass() {
        return prefs
                .get(StringUserPreference.LOOK_AND_FEEL.toString(), "com.jgoodies.looks.plastic.PlasticLookAndFeel");
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
        return prefs.get(StringUserPreference.LOCALE.toString(), "en_GB");
    }

    @Override
    public boolean isAskOverwriteConfirmation() {
        return prefs.getBoolean(BooleanUserPreference.ASK_OVERWRITE_CONFIRMATION.toString(), Boolean.TRUE);
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

    /**
     * Lazy initialization holder class idiom (Joshua Bloch, Effective Java second edition, item 71).
     * 
     * @author Andrea Vacondio
     * 
     */
    private static final class DefaultUserContextHolder {

        private DefaultUserContextHolder() {
            // hide constructor
        }

        static final DefaultUserContext CONTEXT = new DefaultUserContext();
    }

}
