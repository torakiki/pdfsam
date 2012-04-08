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
     * @return true if sounds should played
     */
    boolean isPlaySounds();

    /**
     * @return poolsize of the thumbnails creator framework
     */
    int getThumbnailsCreatorPoolSize();

    /**
     * @return the thumbnails creator identifier
     */
    String getThumbnailsCreatorIdentifier();

    /**
     * @return if updates check should be performed
     */
    boolean isCheckForUpdates();

    /**
     * @return the locale
     */
    String getLocale();

    /**
     * @return true if a confirmation should be asked when an output file is going to be overwritten.
     */
    boolean isAskOverwriteConfirmation();

    // TODO theme and LAF

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

    /**
     * @return the workspaces context for the user
     */
    UserWorkspacesContext getUserWorkspacesContext();

}
