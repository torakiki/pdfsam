/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2012
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

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static java.util.Optional.ofNullable;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Request to save the workspace. Tools will populate the data map with their state in the form of key/value strings.
 *
 * @author Andrea Vacondio
 */
public record SaveWorkspaceRequest(File workspace, Map<String, Map<String, String>> data) {

    public SaveWorkspaceRequest(File workspace, Map<String, Map<String, String>> data) {
        requireNotNullArg(workspace, "Workspace file cannot be null");
        this.workspace = workspace;
        this.data = ofNullable(data).orElseGet(HashMap::new);
    }

    public SaveWorkspaceRequest(File workspace) {
        this(workspace, new ConcurrentHashMap<>());
    }

    /**
     * @param tool
     * @return a Map containing data for the tool or an empty one
     */
    public Map<String, String> getData(String tool) {
        return this.data.computeIfAbsent(tool, k -> new HashMap<>());
    }

}
