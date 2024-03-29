/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12/nov/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.test.ClearEventStudioExtension;

import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ClearEventStudioExtension.class)
@SuppressWarnings("unused")
public class PlaySoundControllerTest {

    @Test
    public void nullSound() {
        assertThrows(IllegalArgumentException.class, () -> new PlaySoundController(null, "something"));
    }

    @Test
    public void nullErrorSound() {
        assertThrows(IllegalArgumentException.class, () -> new PlaySoundController("something", null));
    }

    @Test
    public void blankSound() {
        assertThrows(IllegalArgumentException.class, () -> new PlaySoundController(" ", "something"));
    }

    @Test
    public void blankErrorSound() {
        assertThrows(IllegalArgumentException.class, () -> new PlaySoundController("something", " "));
    }

}
