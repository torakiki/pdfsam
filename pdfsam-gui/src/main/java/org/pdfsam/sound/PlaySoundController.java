/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.media.MediaPlayer;
import javafx.scene.media.MediaPlayer.Status;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.UserContext;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

/**
 * Controller responding to sound related events.
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class PlaySoundController {
    @Inject
    @Named("errorPlayer")
    private MediaPlayer error;
    @Inject
    @Named("okPlayer")
    private MediaPlayer ok;
    @Named
    private UserContext userContext;

    public PlaySoundController() {
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void playFailed(TaskExecutionFailedEvent event) {
        playSound(error);
    }

    @EventListener
    public void playCompleted(TaskExecutionCompletedEvent event) {
        playSound(ok);
    }

    private void playSound(MediaPlayer player) {
        if (userContext.isPlaySounds() && player.getStatus() == Status.READY) {
            player.play();
        }
    }
}
