/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/nov/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
package org.pdfsam.sound;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.context.UserContext;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

/**
 * @author Andrea Vacondio
 *
 */
@SuppressWarnings("unused")
public class PlaySoundControllerTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private UserContext userContext = mock(UserContext.class);

    @Before
    public void setUp() {
        when(userContext.isPlaySounds()).thenReturn(Boolean.FALSE);
    }

    @Test(expected = IllegalArgumentException.class)
    public void blankOk() {
        new PlaySoundController(null, " ", "something");
    }

    @Test(expected = IllegalArgumentException.class)
    public void blankError() {
        new PlaySoundController(null, "something", " ");
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullOk() {
        new PlaySoundController(null, "something", null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullError() {
        new PlaySoundController(null, " ", "something");
    }

    @Test
    public void completeExecutionTriggersSound() {
        PlaySoundController victim = new PlaySoundController(userContext, "something", "something");
        eventStudio().broadcast(new TaskExecutionCompletedEvent(1, null));
        verify(userContext).isPlaySounds();
    }

    @Test
    public void failedExecutionTriggersSound() {
        PlaySoundController victim = new PlaySoundController(userContext, "something", "something");
        eventStudio().broadcast(new TaskExecutionFailedEvent(null, null));
        verify(userContext).isPlaySounds();
    }

}
