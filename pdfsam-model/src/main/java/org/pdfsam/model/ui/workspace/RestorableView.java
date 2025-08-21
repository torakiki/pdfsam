/*
 * This file is part of the PDF Split And Merge source code
 * Created on 06/dic/2014
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
package org.pdfsam.model.ui.workspace;

import org.pdfsam.model.ui.workspace.WorkspaceData.ToolData;

import java.util.Map;

/**
 * A view capable of saving and restoring its state.
 * 
 * @author Andrea Vacondio
 *
 */
public interface RestorableView {

    /**
     * Saves the state to the given data map
     * 
     * @param data
     */
    void saveStateTo(Map<String, String> data);

    /**
     * restore the state from the given data map
     * 
     * @param data
     */
    void restoreStateFrom(ToolData data);
}
