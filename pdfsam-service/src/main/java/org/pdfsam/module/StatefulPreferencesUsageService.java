/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/nov/2013
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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

import java.util.List;
import java.util.Map;
import java.util.prefs.Preferences;
import java.util.stream.Collectors;

import javax.inject.Inject;

/**
 * {@link UsageService} implemented ab/using the {@link Preferences} framework
 * 
 * @author Andrea Vacondio
 * 
 */
class StatefulPreferencesUsageService implements UsageService {

    private PreferencesUsageDataStore dataStore;
    private Map<String, Module> modulesMap;

    @Inject
    StatefulPreferencesUsageService(List<Module> modules, PreferencesUsageDataStore dataStore) {
        this.modulesMap = modules.stream().collect(Collectors.toMap(Module::id, m -> m));
        this.dataStore = dataStore;
    }

    @Override
    public void incrementUsageFor(String moduleId) {
        dataStore.incrementUsageFor(moduleId);
    }

    @Override
    public List<Module> getMostUsed() {
        List<ModuleUsage> used = dataStore.getUsages();
        used.sort((a, b) -> Long.compare(b.getTotalUsed(), a.getTotalUsed()));
        return used.stream().map(u -> modulesMap.get(u.getModuleId())).filter(m -> m != null).collect(toList());
    }

    @Override
    public List<Module> getMostRecentlyUsed() {
        List<ModuleUsage> used = dataStore.getUsages();
        used.sort((a, b) -> Long.compare(b.getLastSeen(), a.getLastSeen()));
        return used.stream().map(u -> modulesMap.get(u.getModuleId())).filter(m -> m != null).collect(toList());
    }

    @Override
    public void clear() {
        dataStore.clear();
    }

    @Override
    public long getTotalUsage() {
        return dataStore.getTotalUsage();
    }

}
