/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
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
     * @return true if sounds should played
     */
    boolean isPlaySounds();

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

    /**
     * @return true if the thumbnails should be rendered with high quality
     */
    boolean isHighQualityThumbnails();

    /**
     * @return the thumbnails creator identifier
     */
    String getThumbnailsCreatorIdentifier();

    /**
     * @return the thumbnails default size
     */
    int getThumbnailsSize();

    /**
     * @return the configured look and feel
     */
    String getLookAndFeelClass();

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
