/*
 * This file is part of the PDF Split And Merge source code
 * Created on 21 gen 2021
 * Copyright 2019 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.tool;

import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;

/**
 * Request to trigger the run button
 *
 * @author Andrea Vacondio
 */
public class RunButtonTriggerRequest {
    public static final RunButtonTriggerRequest INSTANCE = new RunButtonTriggerRequest();
    public static final KeyCodeCombination KEY_CODE_COMBINATION = new KeyCodeCombination(KeyCode.X,
            KeyCombination.SHORTCUT_DOWN);

    private RunButtonTriggerRequest() {
        // hide
    }
}
