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

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Event sent to notify that the user asked to save the workspace. Modules will populate the data map with their state in the form of key/value strings.
 * 
 * @author Andrea Vacondio
 * 
 */
public class SaveWorkspaceEvent extends BaseWorkspaceEvent {
    private Map<String, Map<String, String>> data = new ConcurrentHashMap<>();
    public final boolean awaitCompletion;

    public SaveWorkspaceEvent(File destination) {
        super(destination);
        this.awaitCompletion = false;
    }

    public SaveWorkspaceEvent(File destination, boolean awaitCompletion) {
        super(destination);
        this.awaitCompletion = awaitCompletion;
    }

    /**
     * @return an unmodifiable view of the map
     */
    public Map<String, Map<String, String>> getData() {
        return Collections.unmodifiableMap(data);
    }

    /**
     * Adds the give key/value pair for the given module
     * 
     * @param module
     * @param value
     */
    public void addValue(String module, String key, String value) {
        requireNotNullArg(key, "Destination file cannot be null");
        getDataForModule(module).put(key, defaultString(value));
    }

    /**
     * Null safe get for the module data
     * 
     * @param module
     * @return a list containing data for the module or an empy one
     */
    public Map<String, String> getDataForModule(String module) {
        Map<String, String> values = data.get(module);
        if (values == null) {
            Map<String, String> emptyValues = new HashMap<>();
            values = data.putIfAbsent(module, emptyValues);
            if (values == null) {
                values = emptyValues;
            }
        }
        return values;
    }

}
