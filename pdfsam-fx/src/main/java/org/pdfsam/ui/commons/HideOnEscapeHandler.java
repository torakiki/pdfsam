/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/nov/2013
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

import static org.pdfsam.support.RequireUtils.requireNotNull;

import javafx.event.EventHandler;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.KeyEvent;
import javafx.stage.Window;

/**
 * Scene that hides its Window when the user presses Esc
 * 
 * @author Andrea Vacondio
 * 
 */
public class HideOnEscapeHandler implements EventHandler<KeyEvent> {

    private static final KeyCombination ESCAPE_COMBO = new KeyCodeCombination(KeyCode.ESCAPE);
    private Window window;

    public HideOnEscapeHandler(Window window) {
        requireNotNull(window, "Window cannot be null");
        this.window = window;
    }

    @Override
    public void handle(KeyEvent t) {
        if (window.isShowing() && ESCAPE_COMBO.match(t)) {
            window.hide();
        }
    }
}
