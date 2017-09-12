/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/nov/2012
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import static org.pdfsam.support.RequireUtils.requireNotBlank;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.UserContext;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.injector.Auto;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

import javafx.scene.media.AudioClip;

/**
 * Controller responding to sound related events.
 * 
 * @author Andrea Vacondio
 * 
 */
@Auto
public class PlaySoundController {
    private UserContext userContext;
    private String okSoundURI;
    private String errorSoundURI;

    @Inject
    public PlaySoundController(UserContext userContext, @Named("okSound") String okSoundURI,
            @Named("errorSound") String errorSoundURI) {
        requireNotBlank(okSoundURI, "");
        requireNotBlank(errorSoundURI, "");
        this.userContext = userContext;
        this.okSoundURI = okSoundURI;
        this.errorSoundURI = errorSoundURI;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void playFailed(TaskExecutionFailedEvent event) {
        playSound(errorSoundURI);
    }

    @EventListener
    public void playCompleted(TaskExecutionCompletedEvent event) {
        playSound(okSoundURI);
    }

    private void playSound(String soundURI) {
        if (userContext.isPlaySounds()) {
            new AudioClip(soundURI).play(1);
        }
    }
}
