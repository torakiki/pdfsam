/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/ago/2014
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
package org.pdfsam.module;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.module.ModuleDescriptorBuilder.builder;
import static org.pdfsam.module.ModuleUsage.fistUsage;
import static org.pdfsam.module.ModuleUsage.usage;

import java.util.Arrays;
import java.util.List;

import javax.inject.Named;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.injector.Injector;
import org.sejda.injector.Provides;

/**
 * @author Andrea Vacondio
 *
 */
public class StatefulPreferencesUsageServiceTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private Injector injector;

    @Before
    public void setUp() {
        injector = Injector.start(new Config(), new ModuleServiceConfig());
    }

    static class Config {

        @Provides
        public PreferencesUsageDataStore dataStore() {
            return mock(PreferencesUsageDataStore.class);
        }

        @Provides
        @Named("module1")
        public Module module1() {
            Module module = mock(Module.class);
            when(module.id()).thenReturn("module1");
            ModuleDescriptor descriptor = builder().name("module1").description("desc").category(ModuleCategory.SPLIT)
                    .priority(ModulePriority.DEFAULT).build();
            when(module.descriptor()).thenReturn(descriptor);
            return module;
        }

        @Provides
        @Named("module2")
        public Module module2() {
            Module module = mock(Module.class);
            when(module.id()).thenReturn("module2");
            ModuleDescriptor descriptor = builder().name("module2").description("desc").category(ModuleCategory.MERGE)
                    .priority(ModulePriority.HIGH).build();
            when(module.descriptor()).thenReturn(descriptor);
            return module;
        }
    }

    @Test
    public void clean() {
        injector.instance(StatefulPreferencesUsageService.class).clear();
        verify(injector.instance(PreferencesUsageDataStore.class)).clear();
    }

    @Test
    public void increment() {
        injector.instance(StatefulPreferencesUsageService.class).incrementUsageFor("Chuck");
        verify(injector.instance(PreferencesUsageDataStore.class)).incrementUsageFor("Chuck");
    }

    @Test
    public void getTotalUsage() {
        injector.instance(StatefulPreferencesUsageService.class).getTotalUsage();
        verify(injector.instance(PreferencesUsageDataStore.class)).getTotalUsage();
    }

    @Test
    public void getMostUsed() {
        List<ModuleUsage> usages = Arrays.asList(
                new ModuleUsage[] { fistUsage("IDontExist"), fistUsage("module1"), fistUsage("module2").inc() });
        when(injector.instance(PreferencesUsageDataStore.class).getUsages()).thenReturn(usages);
        List<Module> mostUsed = injector.instance(StatefulPreferencesUsageService.class).getMostUsed();
        assertEquals(2, mostUsed.size());
        assertEquals("module2", mostUsed.get(0).id());
    }

    @Test
    public void getMostRecentlyUsed() {
        List<ModuleUsage> usages = Arrays
                .asList(new ModuleUsage[] { usage("IDontExist", 1), usage("module1", 2), usage("module2", 3) });
        when(injector.instance(PreferencesUsageDataStore.class).getUsages()).thenReturn(usages);
        List<Module> mostUsed = injector.instance(StatefulPreferencesUsageService.class).getMostRecentlyUsed();
        assertEquals(2, mostUsed.size());
        assertEquals("module2", mostUsed.get(0).id());
    }

}
