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
import org.pdfsam.ui.support.Style;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.output.StreamTaskOutput;
import org.sejda.model.output.TaskOutputDispatcher;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.scene.control.Button;

/**
 * Button to open the latest manipulation result
 * 
 * @author Andrea Vacondio
 *
 */
class OpenButton extends Button implements TaskOutputDispatcher {

    private File destination;

    public OpenButton() {
        getStyleClass().addAll(Style.FOOTER_BUTTON.css());
        getStyleClass().add("footer-open-button");
        setText(DefaultI18nContext.getInstance().i18n("Open"));
        setMaxHeight(Double.MAX_VALUE);
        setPrefHeight(Double.MAX_VALUE);
        setOnAction(e -> {
            if (destination != null && destination.exists()) {
                eventStudio().broadcast(new OpenFileRequest(destination));
            }
        });
    }

    public void dispatch(FileTaskOutput output) {
        destination = output.getDestination();
        setGraphic(GlyphsDude.createIcon(FontAwesomeIcon.FILE_PDF_ALT, "1.6em"));
    }

    public void dispatch(DirectoryTaskOutput output) {
        destination = output.getDestination();
        setGraphic(GlyphsDude.createIcon(MaterialDesignIcon.FOLDER_OUTLINE, "1.6em"));
    }

    public void dispatch(StreamTaskOutput output) {
        throw new IllegalArgumentException("Unsupported output type");
    }

}
