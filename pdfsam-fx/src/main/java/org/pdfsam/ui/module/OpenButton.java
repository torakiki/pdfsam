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

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.commons.OpenFileRequest;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.output.StreamTaskOutput;
import org.sejda.model.output.TaskOutputDispatcher;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;

/**
 * Button to open the latest manipulation result
 * 
 * @author Andrea Vacondio
 *
 */
class OpenButton extends Button implements TaskOutputDispatcher {

    private File destination;

    public OpenButton() {
        getStyleClass().addAll("pdfsam-footer-button", "pdfsam-footer-open-button");
        setOnAction(e -> {
            if (destination != null && destination.exists()) {
                eventStudio().broadcast(new OpenFileRequest(destination));
            }
        });
    }

    public void dispatch(FileTaskOutput output) {
        destination = output.getDestination();
        setGraphic(GlyphsDude.createIcon(MaterialDesignIcon.FILE_PDF_BOX, "1.4em"));
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Open the generated document")));
    }

    public void dispatch(DirectoryTaskOutput output) {
        destination = output.getDestination();
        setGraphic(GlyphsDude.createIcon(MaterialDesignIcon.FOLDER_OUTLINE, "1.4em"));
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Open the destination directory")));
    }

    public void dispatch(StreamTaskOutput output) {
        throw new IllegalArgumentException("Unsupported output type");
    }

}
