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
 * {@link Preferences} implementation for the {@link ApplicationContext}.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class DefaultApplicationContext implements ApplicationContext {

    private Preferences prefs;

    private DefaultApplicationContext() {
        this.prefs = Preferences.userNodeForPackage(DefaultApplicationContext.class);
    }

    /**
     * @return the default application context instance
     */
    public static ApplicationContext getInstance() {
        return DefaultApplicationContextHolder.CONTEXT;
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
    public int getThumbnailsCreatorPoolSize() {
        return prefs.getInt(IntUserPreference.THUMBNAILS_SIZE.toString(), 3);
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
    public String getLocale() {
        return prefs.get(StringUserPreference.LOCALE.toString(), StringUtils.EMPTY);
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

    /**
     * Lazy initialization holder class idiom (Joshua Bloch, Effective Java second edition, item 71).
     * 
     * @author Andrea Vacondio
     * 
     */
    private static final class DefaultApplicationContextHolder {

        private DefaultApplicationContextHolder() {
            // hide constructor
        }

        static final DefaultApplicationContext CONTEXT = new DefaultApplicationContext();
    }

}
