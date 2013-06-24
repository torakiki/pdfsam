/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/dic/2011
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
package org.pdfsam.sound;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.DataLine;
import javax.swing.SwingWorker;

import org.pdfsam.context.DefaultI18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Default implementation of a {@link SoundPlayer} playing a sound asynchronously.
 * 
 * @author Andrea Vacondio
 * 
 */
class DefaultSoundPlayer implements SoundPlayer {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultSoundPlayer.class);

    public void play(Sound sound) {
        try {
            AudioInputStream aundioStream = AudioSystem.getAudioInputStream(this.getClass().getResource(
                    sound.getSource()));
            DataLine.Info info = new DataLine.Info(Clip.class, aundioStream.getFormat());
            Clip clip = (Clip) AudioSystem.getLine(info);
            clip.open(aundioStream);
            doPlayAsync(clip);
        } catch (Exception e) {
            LOG.warn(DefaultI18nContext.getInstance().i18n("Error playing sound"), e);
        }
    }

    private void doPlayAsync(final Clip clip) {
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {

            @Override
            public Void doInBackground() {
                clip.setFramePosition(0);
                clip.stop();
                clip.start();
                return null;
            }

        };

        worker.execute();
    }
}
