/*
 * This file is part of the PDF Split And Merge source code
 * Created on 03/apr/2012
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.model.tool;

import javafx.scene.Node;
import javafx.scene.layout.Pane;

/**
 * A PDFsam Basic tool.
 *
 * @author Andrea Vacondio
 */
public interface Tool {

    /**
     * @return the unique id for this tool
     */
    String id();

    /**
     * @return the descriptor for the tool
     */
    ToolDescriptor descriptor();

    /**
     * @return the module panel.
     */
    Pane panel();

    /**
     * @return the graphic node for this {@link Tool}.
     */
    Node graphic();

    /**
     * @return an array containing the required PDF data for this module. Each module can specify data it requires from the PDF document and the PDF load service can use this array
     *         to load only the required data from the PDF, minimizing resources usage.
     */
    default RequiredPdfData[] requires() {
        return new RequiredPdfData[] { RequiredPdfData.DEFAULT };
    }

}
