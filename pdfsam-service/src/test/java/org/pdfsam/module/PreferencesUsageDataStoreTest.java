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

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.List;
import java.util.prefs.Preferences;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.fasterxml.jackson.jr.ob.JSON;
import com.fasterxml.jackson.jr.ob.JSONObjectException;

/**
 * @author Andrea Vacondio
 *
 */
public class PreferencesUsageDataStoreTest {

    private PreferencesUsageDataStore victim = new PreferencesUsageDataStore();

    @After
    @Before
    public void clear() {
        victim.clear();
    }

    @Test
    public void incrementUsageFor() throws JSONObjectException, IOException {
        victim.incrementUsageFor("moduleId");
        ModuleUsage usage = JSON.std.beanFrom(
                ModuleUsage.class,
                Preferences.userRoot().node(PreferencesUsageDataStore.USAGE_PATH).node("moduleId")
                        .get(PreferencesUsageDataStore.MODULE_USAGE_KEY, ""));
        assertEquals(1, usage.getTotalUsed());
        assertTrue(usage.getLastSeen() != 0);
    }

    @Test
    public void multipleIncrementUsageFor() throws JSONObjectException, IOException, InterruptedException {
        victim.incrementUsageFor("moduleId");
        ModuleUsage usage = JSON.std.beanFrom(
                ModuleUsage.class,
                Preferences.userRoot().node(PreferencesUsageDataStore.USAGE_PATH).node("moduleId")
                        .get(PreferencesUsageDataStore.MODULE_USAGE_KEY, ""));
        Thread.sleep(100);
        victim.incrementUsageFor("moduleId");
        ModuleUsage usage2 = JSON.std.beanFrom(
                ModuleUsage.class,
                Preferences.userRoot().node(PreferencesUsageDataStore.USAGE_PATH).node("moduleId")
                        .get(PreferencesUsageDataStore.MODULE_USAGE_KEY, ""));
        assertEquals(2, usage2.getTotalUsed());
        assertTrue(usage.getLastSeen() != usage2.getLastSeen());
    }

    @Test
    public void testClear() {
        victim.incrementUsageFor("moduleId");
        victim.clear();
        assertTrue(isBlank(Preferences.userRoot().node(PreferencesUsageDataStore.USAGE_PATH).node("moduleId")
                .get(PreferencesUsageDataStore.MODULE_USAGE_KEY, "")));
    }

    @Test
    public void multipleClear() {
        victim.incrementUsageFor("moduleId");
        victim.clear();
        victim.incrementUsageFor("moduleId");
        victim.clear();
        assertTrue(isBlank(Preferences.userRoot().node(PreferencesUsageDataStore.USAGE_PATH).node("moduleId")
                .get(PreferencesUsageDataStore.MODULE_USAGE_KEY, "")));
    }

    @Test
    public void getUsages() {
        victim.incrementUsageFor("module1");
        victim.incrementUsageFor("module1");
        victim.incrementUsageFor("module2");
        List<ModuleUsage> result = victim.getUsages();
        assertEquals(2, result.size());
    }
}
