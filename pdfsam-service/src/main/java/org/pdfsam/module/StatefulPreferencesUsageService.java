/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static java.util.stream.Collectors.toList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javax.annotation.PreDestroy;
import javax.inject.Inject;
import javax.inject.Named;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link UsageService} implemented ab/using the {@link Preferences} framework
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class StatefulPreferencesUsageService implements UsageService {

    private static final Logger LOG = LoggerFactory.getLogger(StatefulPreferencesUsageService.class);

    private static final String LAST_SEEN_KEY = "last.seen";
    private static final String TOTAL_USED_KEY = "total.used";
    private Map<String, ModuleUsage> modules = new HashMap<>();
    private Preferences prefs = Preferences.userRoot().node("/pdfsam/modules/usage");

    @Inject
    public StatefulPreferencesUsageService(Map<String, Module> modulesMap) {
        for (Module module : modulesMap.values()) {
            ModuleUsage usage = new ModuleUsage();
            usage.module = module;
            Preferences node = prefs.node(module.id());
            usage.lastSeen = node.getLong(LAST_SEEN_KEY, 0);
            usage.totalUsed = node.getLong(TOTAL_USED_KEY, 0);
            modules.put(module.id(), usage);
        }
    }

    public void incrementUsageFor(String moduleId) {
        ModuleUsage usage = modules.get(moduleId);
        if (usage != null) {
            usage.totalUsed += 1;
            usage.lastSeen = System.currentTimeMillis();
            LOG.trace("Set total usage to {} for module {}", usage.totalUsed, moduleId);
        }
    }

    public List<Module> getMostUsed() {
        List<ModuleUsage> used = modules.values().parallelStream().collect(toList());
        used.sort((a, b) -> {
            if (a.totalUsed == 0 && b.totalUsed == 0) {
                return Integer.compare(a.module.descriptor().getPriority(), b.module.descriptor().getPriority());
            }
            return Long.compare(b.totalUsed, a.totalUsed);
        });
        return used.stream().map(u -> u.module).collect(toList());
    }

    public List<Module> getMostRecentlyUsed() {
        List<ModuleUsage> used = modules.values().parallelStream().filter(t -> t.lastSeen != 0).collect(toList());
        used.sort((a, b) -> {
            return Long.compare(b.lastSeen, a.lastSeen);
        });
        return used.stream().map(u -> u.module).collect(toList());
    }

    public void clear() {
        try {
            prefs.clear();
            modules.values().parallelStream().forEach(u -> {
                u.lastSeen = 0;
                u.totalUsed = 0;
            });
        } catch (BackingStoreException e) {
            LOG.error("Unable to clear modules usage statistics", e);
        }
    }

    /**
     * flush to the persistence backing store the current state of the usage
     */
    @PreDestroy
    private void flush() {
        try {
            prefs.clear();
            for (ModuleUsage entry : modules.values()) {
                Preferences node = prefs.node(entry.module.id());
                node.putLong(LAST_SEEN_KEY, entry.lastSeen);
                node.putLong(TOTAL_USED_KEY, entry.totalUsed);
            }
            LOG.trace("Flush of usage statistics performed for {} modules", modules.size());
        } catch (BackingStoreException e) {
            LOG.error("Unable to persist modules usage statistics", e);
        }
    }

    /**
     * Helper class used to bind {@link Module}s and their usage statistics
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class ModuleUsage {
        private Module module;
        private long lastSeen = 0;
        private long totalUsed = 0;

    }
}
