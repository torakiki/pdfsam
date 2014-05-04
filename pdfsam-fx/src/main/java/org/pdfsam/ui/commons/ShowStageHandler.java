/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.stage.Stage;

/**
 * Handler for an action showing a {@link Stage}
 * 
 * @author Andrea Vacondio
 */
public class ShowStageHandler implements EventHandler<ActionEvent> {

    private Stage stage;

    public ShowStageHandler(Stage stage) {
        requireNotNull(stage, "Stage cannot be null");
        this.stage = stage;
    }

    public void handle(ActionEvent event) {
        if (!stage.isShowing()) {
            stage.centerOnScreen();
            stage.show();
        }
        stage.requestFocus();
    }
}