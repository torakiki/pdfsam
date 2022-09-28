/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/lug/2014
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
package org.pdfsam.ui.commons;

import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.stage.Stage;
import javafx.stage.Window;

/**
 * @author Andrea Vacondio
 *
 */
public class HideOnEscapeHandlerTest extends ApplicationTest {
    private Scene scene;

    @Override
    public void start(Stage stage) {
        scene = new Scene(new Label("me"));
        stage.setScene(scene);
        stage.show();
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArg() {
        new HideOnEscapeHandler(null);
    }

    @Test
    public void notShowingNoMatch() {
        Window window = spy(new Window() {
        });
        HideOnEscapeHandler victim = new HideOnEscapeHandler(window);
        victim.handle(
                new KeyEvent(KeyEvent.KEY_RELEASED, KeyCode.A.toString(), "", KeyCode.A, false, false, false, false));
        verify(window, never()).hide();
    }

    @Test
    public void showingMatch() {
        Window window = spy(window(scene));
        HideOnEscapeHandler victim = new HideOnEscapeHandler(window);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.handle(new KeyEvent(KeyEvent.KEY_RELEASED,
                KeyCode.ESCAPE.toString(), "", KeyCode.ESCAPE, false, false, false, false)));
        verify(window, times(1)).hide();
    }

    @Test
    public void notShowingMatch() {
        Window window = spy(new Window() {
        });
        HideOnEscapeHandler victim = new HideOnEscapeHandler(window);
        victim.handle(new KeyEvent(KeyEvent.KEY_RELEASED, KeyCode.ESCAPE.toString(), "", KeyCode.ESCAPE, false, false,
                false, false));
        verify(window, never()).hide();
    }
}
