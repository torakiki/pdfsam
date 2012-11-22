/*
 * Created on 22/nov/2012
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
package org.pdfsam.sound;

import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.context.DefaultUserContext;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

/**
 * Controller responding to sound related events.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PlaySoundController {

    private SoundPlayer player = new DefaultSoundPlayer();

    @EventSubscriber
    public void playFailed(TaskExecutionFailedEvent event) {
        playSound(Sound.NEGATIVE);
    }

    @EventSubscriber
    public void playCompleted(TaskExecutionCompletedEvent event) {
        playSound(Sound.POSITIVE);
    }

    private void playSound(Sound sound) {
        if (DefaultUserContext.getInstance().isPlaySounds()) {
            player.play(sound);
        }
    }
}
