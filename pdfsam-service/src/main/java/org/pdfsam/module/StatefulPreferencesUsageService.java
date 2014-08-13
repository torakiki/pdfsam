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

import java.util.List;
import java.util.Map;
import java.util.prefs.Preferences;

import javax.inject.Inject;
import javax.inject.Named;

/**
 * {@link UsageService} implemented ab/using the {@link Preferences} framework
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class StatefulPreferencesUsageService implements UsageService {

    @Inject
    private PreferencesUsageDataStore dataStore;
    @Inject
    private Map<String, Module> modulesMap;

    public void incrementUsageFor(String moduleId) {
        dataStore.incrementUsageFor(moduleId);
    }

    public List<Module> getMostUsed() {
        List<ModuleUsage> used = dataStore.getUsages();
        used.sort((a, b) -> {
            return Long.compare(b.getTotalUsed(), a.getTotalUsed());
        });
        return used.stream().map(u -> modulesMap.get(u.getModuleId())).filter(m -> m != null).collect(toList());
    }

    public List<Module> getMostRecentlyUsed() {
        List<ModuleUsage> used = dataStore.getUsages();
        used.sort((a, b) -> {
            return Long.compare(b.getLastSeen(), a.getLastSeen());
        });
        return used.stream().map(u -> modulesMap.get(u.getModuleId())).filter(m -> m != null).collect(toList());
    }

    public void clear() {
        dataStore.clear();
    }

}
