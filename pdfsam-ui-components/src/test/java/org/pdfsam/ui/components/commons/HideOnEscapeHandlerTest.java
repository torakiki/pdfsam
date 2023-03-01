/*
 * This file is part of the PDF Split And Merge source code
 * Created on 07/lug/2014
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
package org.pdfsam.ui.components.commons;

import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.stage.Stage;
import javafx.stage.Window;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.testfx.assertions.api.Assertions.assertThat;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
public class HideOnEscapeHandlerTest {
    private Scene scene;

    @Start
    private void start(Stage stage) {
        scene = new Scene(new Label("me"));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void nullArg() {
        assertThrows(IllegalArgumentException.class, () -> new HideOnEscapeHandler(null));
    }

    @Test
    public void notShowingNoMatch(FxRobot robot) {
        Window window = robot.window(scene);
        var victim = new HideOnEscapeHandler(window);
        victim.handle(
                new KeyEvent(KeyEvent.KEY_RELEASED, KeyCode.A.toString(), "", KeyCode.A, false, false, false, false));
        assertThat(window).isShowing();
    }

    @Test
    public void showingMatch(FxRobot robot) {
        Window window = robot.window(scene);
        var victim = new HideOnEscapeHandler(window);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.handle(
                new KeyEvent(KeyEvent.KEY_RELEASED, KeyCode.ESCAPE.toString(), "", KeyCode.ESCAPE, false, false, false,
                        false)));
        assertThat(window).isNotShowing();
    }

    @Test
    public void notShowingMatch(FxRobot robot) {
        Window window = robot.window(scene);

        var victim = new HideOnEscapeHandler(window);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            window.hide();
            victim.handle(
                    new KeyEvent(KeyEvent.KEY_RELEASED, KeyCode.ESCAPE.toString(), "", KeyCode.ESCAPE, false, false,
                            false, false));
        });
        assertThat(window).isNotShowing();
    }
}
