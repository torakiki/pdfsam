/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/ago/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
import static org.apache.commons.lang3.StringUtils.isNotBlank;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javax.annotation.PreDestroy;
import javax.inject.Named;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.jr.ob.JSON;

/**
 * @author Andrea Vacondio
 *
 */
@Named
class PreferencesUsageDataStore {

    private static final Logger LOG = LoggerFactory.getLogger(PreferencesUsageDataStore.class);
    static final String USAGE_PATH = "/pdfsam/modules/usage";
    static final String MODULE_USAGE_KEY = "module.usage";

    @PreDestroy
    public void flush() {
        Preferences prefs = Preferences.userRoot().node(USAGE_PATH);
        try {
            prefs.flush();
        } catch (BackingStoreException e) {
            LOG.error("Unable to flush modules usage statistics", e);
        }
    }

    public void incrementUsageFor(String moduleId) {
        Preferences node = Preferences.userRoot().node(USAGE_PATH).node(moduleId);
        String json = node.get(MODULE_USAGE_KEY, "");
        try {
            if (isNotBlank(json)) {
                node.put(MODULE_USAGE_KEY, JSON.std.asString(JSON.std.beanFrom(ModuleUsage.class, json).inc()));
            } else {
                node.put(MODULE_USAGE_KEY, JSON.std.asString(ModuleUsage.fistUsage(moduleId)));
            }
            LOG.trace("Usage incremented for module {}", moduleId);
        } catch (IOException e) {
            LOG.error("Unable to increment modules usage statistics", e);
        }
    }

    public List<ModuleUsage> getUsages() {
        Preferences prefs = Preferences.userRoot().node(USAGE_PATH);
        List<ModuleUsage> retList = new ArrayList<>();
        try {
            List<String> jsons = Arrays.stream(prefs.childrenNames()).parallel().map(name -> prefs.node(name))
                    .map(node -> node.get(MODULE_USAGE_KEY, "")).filter(json -> isNotBlank(json)).collect(toList());
            for (String json : jsons) {
                retList.add(JSON.std.beanFrom(ModuleUsage.class, json));
            }
        } catch (BackingStoreException | IOException e) {
            LOG.error("Unable to get modules usage statistics", e);
        }
        return retList;
    }

    public void clear() {
        Preferences prefs = Preferences.userRoot().node(USAGE_PATH);
        try {
            prefs.removeNode();
            prefs.flush();
        } catch (BackingStoreException e) {
            LOG.error("Unable to clear modules usage statistics", e);
        }
    }

}
