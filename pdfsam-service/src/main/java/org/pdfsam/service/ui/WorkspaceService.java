/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/dic/2014
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
package org.pdfsam.service.ui;

import java.io.File;
import java.util.Map;

/**
 * Services related to workspaces
 * 
 * @author Andrea Vacondio
 *
 */
public interface WorkspaceService {

    /**
     * Saves the given map of data to the destination file.
     * 
     * @param data
     *            map of maps. The key is a module id and the value is a key/value map used by the module to store its state
     * @param destination
     * @throws RuntimeException
     *             in case of error
     */
    void saveWorkspace(Map<String, Map<String, String>> data, File destination);

    /**
     * Loads the workspace from the given file
     * 
     * @param workspace
     * @return a map of maps. The key is a module id and the value is a key/value map used by the module to store its state
     * @throws RuntimeException
     *             in case of error
     */
    Map<String, Map<String, String>> loadWorkspace(File workspace);
}
