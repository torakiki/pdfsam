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

import org.junit.Assert;
import org.junit.Test;

/**
 * Test unit for the {@link DefaultUserContext}.
 * 
 * @author Andrea Vacondio
 * 
 */
public class DefaultUserContextTest {

    @Test
    public void isAskConfirmation() {
        DefaultUserContext.getInstance().setBooleanPreference(BooleanUserPreference.ASK_OVERWRITE_CONFIRMATION, false);
        Assert.assertFalse(DefaultUserContext.getInstance().isAskOverwriteConfirmation());
    }

    @Test
    public void isCheckUpdates() {
        DefaultUserContext.getInstance().setBooleanPreference(BooleanUserPreference.CHECK_UPDATES, false);
        Assert.assertFalse(DefaultUserContext.getInstance().isCheckForUpdates());
    }

    @Test
    public void isAskPlaySounds() {
        DefaultUserContext.getInstance().setBooleanPreference(BooleanUserPreference.PLAY_SOUNDS, false);
        Assert.assertFalse(DefaultUserContext.getInstance().isPlaySounds());
    }

    @Test
    public void getLocale() {
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.LOCALE, "en");
        Assert.assertEquals("en", DefaultUserContext.getInstance().getLocale());
    }

    @Test
    public void getThumbIdentifier() {
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.THUMBNAILS_IDENTIFIER, "ChuckNorris");
        Assert.assertEquals("ChuckNorris", DefaultUserContext.getInstance().getThumbnailsCreatorIdentifier());
    }

    @Test
    public void getDefaultWorkingPath() {
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.WORKING_PATH, "/path");
        Assert.assertEquals("/path", DefaultUserContext.getInstance().getDefaultWorkingPath());
    }

    @Test
    public void getDefaultWorkspacePath() {
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.WORKSPACE_PATH, "/wpath");
        Assert.assertEquals("/wpath", DefaultUserContext.getInstance().getDefaultWorkspacePath());
    }

    @Test
    public void getThumbPoolSize() {
        DefaultUserContext.getInstance().setIntegerPreference(IntUserPreference.THUMBNAILS_SIZE, 2);
        Assert.assertEquals(2, DefaultUserContext.getInstance().getThumbnailsSize());
    }
}
