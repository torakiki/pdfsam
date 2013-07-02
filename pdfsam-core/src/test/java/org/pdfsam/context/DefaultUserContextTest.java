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
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.LOCALE, "en-GB");
        Assert.assertEquals("en-GB", DefaultUserContext.getInstance().getLocale());
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.LOCALE, "");
    }

    @Test
    public void getThumbIdentifier() {
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.THUMBNAILS_IDENTIFIER, "ChuckNorris");
        Assert.assertEquals("ChuckNorris", DefaultUserContext.getInstance().getThumbnailsCreatorIdentifier());
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.THUMBNAILS_IDENTIFIER, "");
    }

    @Test
    public void getDefaultWorkingPath() {
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.WORKING_PATH, "/path");
        Assert.assertEquals("/path", DefaultUserContext.getInstance().getDefaultWorkingPath());
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.WORKING_PATH, "");
    }

    @Test
    public void getDefaultWorkspacePath() {
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.WORKSPACE_PATH, "/wpath");
        Assert.assertEquals("/wpath", DefaultUserContext.getInstance().getDefaultWorkspacePath());
        DefaultUserContext.getInstance().setStringPreference(StringUserPreference.WORKSPACE_PATH, "");
    }

    @Test
    public void getThumbPoolSize() {
        DefaultUserContext.getInstance().setIntegerPreference(IntUserPreference.THUMBNAILS_SIZE, 2);
        Assert.assertEquals(2, DefaultUserContext.getInstance().getThumbnailsSize());
    }
}
