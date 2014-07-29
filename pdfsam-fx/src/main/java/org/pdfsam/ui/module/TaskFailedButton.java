/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/lug/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.module;

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.control.Button;

import org.pdfsam.ui.commons.ShowStageRequest;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Button requesting to open the log stage to show the user possible errors
 * 
 * @author Andrea Vacondio
 *
 */
class TaskFailedButton extends Button {

    public TaskFailedButton() {
        getStyleClass().addAll("pdfsam-footer-button", "pdfsam-footer-failed-button");
        setGraphic(AwesomeDude.createIconLabel(AwesomeIcon.TIMES_CIRCLE));
        setOnAction(e -> eventStudio().broadcast(new ShowStageRequest(), "LogStage"));
    }
}
