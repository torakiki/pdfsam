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

/**
 * Context holding user related application settings.
 * 
 * @author Andrea Vacondio
 * 
 */
public interface UserContext {

    /**
     * @return the default workspace path if set, an empty string if not set.
     */
    String getDefaultWorkspacePath();

    /**
     * @return the default working path if set, an empty string if not set.
     */
    String getDefaultWorkingPath();

    /**
     * @return true if modules should set the output file/folder as the file/folder of the selected input PDF
     */
    boolean isUseSmartOutput();

    /**
     * @return true if sounds should played
     */
    boolean isPlaySounds();

    /**
     * @return true if the notification should be shown
     */
    boolean isDonationNotification();

    /**
     * @return true if premium modules description should be fetched and shown
     */
    boolean isFetchPremiumModules();

    /**
     * @return if updates check should be performed
     */
    boolean isCheckForUpdates();

    /**
     * @return if news check should be performed
     */
    boolean isCheckForNews();

    /**
     * @return if the output PDF file compression settings should be enabled by default
     */
    boolean isCompressionEnabled();

    /**
     * @return true if documents password should be saved when saving a workspace
     */
    boolean isSavePwdInWorkspaceFile();

    /**
     * @return if workspace should be saved on application exit
     */
    boolean isSaveWorkspaceOnExit();

    /**
     * @return the locale
     */
    String getLocale();

    /**
     * @return the max number of rows that the log view should display
     */
    int getNumberOfLogRows();

    /**
     * @return the module to load at application startup
     */
    String getStartupModule();

    /**
     * @param module
     * @return the value of the default prefix for the given module
     */
    String getDefaultPrefix(String module);

    /**
     * Sets the default value for the prefix of a given module
     * 
     * @param module
     * @param value
     */
    void setDefaultPrefix(String module, String value);

    /**
     * Clear user preferences
     */
    void clear();

    /**
     * Sets a {@link Boolean} preference.
     * 
     * @param pref
     * @param value
     */
    void setBooleanPreference(BooleanUserPreference pref, boolean value);

    /**
     * Sets an {@link Integer} preference.
     * 
     * @param pref
     * @param value
     */
    void setIntegerPreference(IntUserPreference pref, int value);

    /**
     * Sets a {@link String} preference.
     * 
     * @param pref
     * @param value
     */
    void setStringPreference(StringUserPreference pref, String value);

}
