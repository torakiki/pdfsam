/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08/ott/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.model.ui;

import javafx.stage.Stage;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 */
public class StageModeTest {

    @Test
    @Disabled("Needs to be in JavaFX thread")
    public void valueForMaximized() {
        var stage = mock(Stage.class);
        when(stage.isMaximized()).thenReturn(true);
        assertEquals(StageMode.MAXIMIZED, StageMode.valueFor(stage));
    }

    @Test
    @Disabled("Needs to be in JavaFX thread")
    public void valueForDefault() {
        var stage = mock(Stage.class);
        when(stage.isMaximized()).thenReturn(false);
        assertEquals(StageMode.DEFAULT, StageMode.valueFor(stage));
    }

    @Test
    public void restoreDefault() {
        var stage = mock(Stage.class);
        StageMode.DEFAULT.restore(stage);
        verifyNoInteractions(stage);
    }

    @Test
    public void restoreMaximized() {
        var stage = mock(Stage.class);
        StageMode.MAXIMIZED.restore(stage);
        verify(stage, Mockito.times(1)).setMaximized(true);
    }
}
