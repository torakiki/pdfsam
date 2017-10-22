/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2012
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.workspace;

import static java.util.Objects.requireNonNull;
import static java.util.Optional.ofNullable;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

/**
 * Event to notify that the user asked to load a workspace.
 * 
 * @author Andrea Vacondio
 * 
 */
public class LoadWorkspaceEvent extends BaseWorkspaceEvent {
    private Map<String, Map<String, String>> data = new HashMap<>();

    public LoadWorkspaceEvent(File workspace) {
        super(workspace);
    }

    public void setData(Map<String, Map<String, String>> data) {
        requireNonNull(data);
        this.data.putAll(data);
    }

    public Map<String, String> getData(String module) {
        return ofNullable(this.data.get(module)).orElseGet(HashMap::new);
    }

}
