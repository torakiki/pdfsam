/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2012
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
package org.pdfsam.context;

import static java.util.Collections.reverse;
import static java.util.Collections.unmodifiableList;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.support.RequireUtils.require;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import org.pdfsam.support.LRUMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link UserWorkspacesContext} implementation using {@link Preferences}.
 * 
 * @author Andrea Vacondio
 * 
 */
class PreferencesUserWorkspacesContext implements UserWorkspacesContext {
    private static final Logger LOG = LoggerFactory.getLogger(PreferencesUserWorkspacesContext.class);
    static final int MAX_CAPACITY = 5;

    private Map<String, String> cache = new LRUMap<>(MAX_CAPACITY);
    private Preferences prefs;

    public PreferencesUserWorkspacesContext() {
        this.prefs = Preferences.userRoot().node("/pdfsam/user/workspaces");
        populateCache();
    }

    private void populateCache() {
        try {
            SortedSet<String> keys = new TreeSet<>(Arrays.asList(prefs.keys()));
            for (String key : keys) {
                String currentValue = prefs.get(key, EMPTY);
                if (isNotBlank(currentValue)) {
                    cache.put(key, currentValue);
                }
            }
        } catch (BackingStoreException e) {
            LOG.error("Error getting recently used workspaces", e);
        }
    }

    @Override
    public void addWorkspace(String workspace) {
        require(isNotBlank(workspace), "Blank workspace is not allowed");
        cache.put(Long.toString(System.currentTimeMillis()), workspace);
        try {
            prefs.clear();
            for (Entry<String, String> entry : cache.entrySet()) {
                prefs.put(entry.getKey(), entry.getValue());
            }
        } catch (BackingStoreException e) {
            LOG.error("Error storing recently used workspace", e);
        }
    }

    @Override
    public List<String> getWorkspaces() {
        List<String> values = new ArrayList<>(cache.values());
        reverse(values);
        return unmodifiableList(values);
    }
}
