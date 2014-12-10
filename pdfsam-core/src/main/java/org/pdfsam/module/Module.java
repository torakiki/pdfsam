/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/apr/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.module;

import java.util.Map;

import javafx.scene.Node;
import javafx.scene.layout.Pane;

/**
 * pdfsam module.
 * 
 * @author Andrea Vacondio
 * 
 */
public interface Module {

    /**
     * @return the unique id for this module
     */
    String id();

    /**
     * @return the descriptor for the module
     */
    ModuleDescriptor descriptor();

    /**
     * @return the module panel.
     */
    Pane modulePanel();

    /**
     * @return the graphic node for this {@link Module}.
     */
    Node graphic();

    /**
     * Request to add the module state to the given data map in a ChainOfResponsability fashion.
     * 
     * @param data
     */
    void onSaveWorkspace(Map<String, String> data);

    /**
     * Request to restore the module state using the provided data.
     * 
     * @param data
     */
    void onLoadWorkspace(Map<String, String> data);

    /**
     * @return an array containing the required PDF data for this module. Each module can specify data it requires from the PDF document and the PDF load service can use this array
     *         to load only the required data from the PDF, minimizing resources usage.
     */
    default RequiredPdfData[] requires() {
        return new RequiredPdfData[] { RequiredPdfData.DEFAULT };
    }

}
