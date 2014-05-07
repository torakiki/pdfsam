/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/mar/2014
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

import java.io.File;

import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.event.OpenFileRequestEvent;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.output.StreamTaskOutput;
import org.sejda.model.output.TaskOutputDispatcher;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Button to open the latest manipulation result
 * 
 * @author Andrea Vacondio
 *
 */
class OpenButton extends Button implements TaskOutputDispatcher {

    private File destination;

    public OpenButton() {
        getStyleClass().add("pdfsam-footer-button");
        setGraphic(AwesomeDude.createIconLabel(AwesomeIcon.FOLDER_OPEN));
        setOnAction(e -> {
            if (destination != null && destination.exists()) {
                eventStudio().broadcast(new OpenFileRequestEvent(destination));
            }
        });
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Open the generated file/files")));
    }

    public void dispatch(FileTaskOutput output) {
        destination = output.getDestination();
    }

    public void dispatch(DirectoryTaskOutput output) {
        destination = output.getDestination();
    }

    public void dispatch(StreamTaskOutput output) {
        throw new IllegalArgumentException("Unsupported output type");
    }

}
