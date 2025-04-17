/*
 * This file is part of the PDF Split And Merge source code
 * Created on 11/dic/2014
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

import jakarta.inject.Inject;
import jakarta.inject.Named;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.model.lifecycle.ShutdownEvent;
import org.pdfsam.persistence.PersistenceException;
import org.pdfsam.persistence.PreferencesRepository;
import org.sejda.commons.collection.LRUMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static java.util.Collections.reverseOrder;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Default implementation of the {@link RecentWorkspacesService}.
 *
 * @author Andrea Vacondio
 */
public class DefaultRecentWorkspacesService implements RecentWorkspacesService {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultRecentWorkspacesService.class);

    static final int MAX_CAPACITY = 20;
    private final PreferencesRepository repo;
    private final Map<String, String> cache = new LRUMap<>(MAX_CAPACITY);

    @Inject
    public DefaultRecentWorkspacesService(@Named("recentWorkspacesRepository") PreferencesRepository repo) {
        this.repo = repo;
        populateCache();
        eventStudio().addAnnotatedListeners(this);
    }

    private void populateCache() {
        try {
            Arrays.stream(repo.keys()).sorted().forEach(k -> {
                String currentValue = repo.getString(k, EMPTY);
                if (isNotBlank(currentValue)) {
                    cache.put(currentValue, k);
                }
            });
        } catch (PersistenceException e) {
            LOG.error("Error retrieving recently used workspaces", e);
        }
    }

    @Override
    public void addWorkspaceLastUsed(File workspace) {
        requireNotNullArg(workspace, "Null workspace is not allowed");
        cache.put(workspace.getAbsolutePath(), Long.toString(Instant.now().toEpochMilli()));
        LOG.trace("Added recently used workspace {}", workspace.getAbsolutePath());
    }

    @Override
    public List<String> getRecentlyUsedWorkspaces() {
        return cache.entrySet().stream().sorted(Map.Entry.comparingByValue(reverseOrder())).map(Map.Entry::getKey)
                .toList();
    }

    @Override
    public void clear() {
        this.cache.clear();
        try {
            this.repo.clean();
        } catch (PersistenceException e) {
            LOG.error("Unable to clear recently used workspaces", e);
        }
    }

    @EventListener
    public void onShutdown(ShutdownEvent event) {
        LOG.trace("Flushing recently used workspaces");
        try {
            this.repo.clean();
            this.cache.forEach((k, v) -> this.repo.saveString(v, k));
        } catch (PersistenceException e) {
            LOG.error("Error storing recently used workspace", e);
        }
    }

}
