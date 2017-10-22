/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/dic/2014
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
package org.pdfsam.ui;

import static java.util.Collections.reverse;
import static java.util.Collections.unmodifiableList;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.support.RequireUtils.requireNotNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import org.pdfsam.ShutdownEvent;
import org.pdfsam.support.LRUMap;
import org.sejda.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implementation of the {@link RecentWorkspacesService} using the {@link Preferences} framework.
 * 
 * @author Andrea Vacondio
 *
 */
class PreferencesRecentWorkspacesService implements RecentWorkspacesService {

    private static final Logger LOG = LoggerFactory.getLogger(PreferencesRecentWorkspacesService.class);

    static final int MAX_CAPACITY = 5;
    static final String WORKSPACES_PATH = "/org/pdfsam/user/workspaces";

    private Map<String, String> cache = new LRUMap<>(MAX_CAPACITY);

    public PreferencesRecentWorkspacesService() {
        populateCache();
        eventStudio().addAnnotatedListeners(this);
    }

    private void populateCache() {
        Preferences prefs = Preferences.userRoot().node(WORKSPACES_PATH);
        try {
            Arrays.stream(prefs.keys()).sorted().forEach(k -> {
                String currentValue = prefs.get(k, EMPTY);
                if (isNotBlank(currentValue)) {
                    cache.put(currentValue, k);
                }
            });
        } catch (BackingStoreException e) {
            LOG.error("Error retrieving recently used workspaces", e);
        }
    }

    @Override
    public void addWorkspaceLastUsed(File workspace) {
        requireNotNull(workspace, "Null workspace is not allowed");
        cache.put(workspace.getAbsolutePath(), Long.toString(Instant.now().toEpochMilli()));
        LOG.trace("Added recently used workspace {}", workspace.getAbsolutePath());
    }

    @Override
    public List<String> getRecentlyUsedWorkspaces() {
        List<String> values = new ArrayList<>(cache.keySet());
        reverse(values);
        return unmodifiableList(values);
    }

    @Override
    public void clear() {
        Preferences prefs = Preferences.userRoot().node(WORKSPACES_PATH);
        cache.clear();
        try {
            prefs.removeNode();
            prefs.flush();
        } catch (BackingStoreException e) {
            LOG.error("Unable to clear recently used workspaces", e);
        }
    }

    public void flush() {
        Preferences prefs = Preferences.userRoot().node(WORKSPACES_PATH);
        LOG.trace("Flushing recently used workspaces");
        try {
            prefs.clear();
            for (Entry<String, String> entry : cache.entrySet()) {
                prefs.put(entry.getValue(), entry.getKey());
            }
            prefs.flush();
        } catch (BackingStoreException e) {
            LOG.error("Error storing recently used workspace", e);
        }
    }

    @EventListener
    public void onShutdown(ShutdownEvent event) {
        flush();
    }

}
