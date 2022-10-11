/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22/nov/2012
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
package org.pdfsam.gui.sound;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.scene.media.AudioClip;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * Controller responding to sound related events.
 *
 * @author Andrea Vacondio
 */
@Auto
public class PlaySoundController {
    private final String okSoundURI;
    private final String errorSoundURI;

    @Inject
    public PlaySoundController(@Named("okSound") String okSoundURI, @Named("errorSound") String errorSoundURI) {
        requireNotBlank(okSoundURI, "");
        requireNotBlank(errorSoundURI, "");
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
        if (app().persistentSettings().get(BooleanPersistentProperty.PLAY_SOUNDS)) {
            new AudioClip(soundURI).play(1);
        }
    }
}
