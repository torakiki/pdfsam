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

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.module.ModuleDescriptorBuilder.builder;
import static org.pdfsam.module.ModuleUsage.fistUsage;
import static org.pdfsam.module.ModuleUsage.usage;

import java.util.Arrays;
import java.util.List;

import javax.inject.Inject;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.pdfsam.test.ClearEventStudioRule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class StatefulPreferencesUsageServiceTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    @Inject
    private StatefulPreferencesUsageService victim;
    @Inject
    private PreferencesUsageDataStore dataStore;

    @Configuration
    @ComponentScan("org.pdfsam.module")
    static class Config {

        @Bean
        @Primary
        public PreferencesUsageDataStore dataStore() {
            return mock(PreferencesUsageDataStore.class);
        }

        @Bean
        public Module module1() {
            Module module = mock(Module.class);
            when(module.id()).thenReturn("module1");
            ModuleDescriptor descriptor = builder().name("module1").description("desc").category(ModuleCategory.SPLIT)
                    .priority(ModulePriority.DEFAULT).build();
            when(module.descriptor()).thenReturn(descriptor);
            return module;
        }

        @Bean
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
        victim.clear();
        verify(dataStore).clear();
    }

    @Test
    public void increment() {
        victim.incrementUsageFor("Chuck");
        verify(dataStore).incrementUsageFor("Chuck");
    }

    @Test
    public void getMostUsed() {
        List<ModuleUsage> usages = Arrays.asList(new ModuleUsage[] { fistUsage("IDontExist"), fistUsage("module1"),
                fistUsage("module2").inc() });
        when(dataStore.getUsages()).thenReturn(usages);
        List<Module> mostUsed = victim.getMostUsed();
        assertEquals(2, mostUsed.size());
        assertEquals("module2", mostUsed.get(0).id());
    }

    @Test
    public void getMostRecentlyUsed() {
        List<ModuleUsage> usages = Arrays.asList(new ModuleUsage[] { usage("IDontExist", 1), usage("module1", 2),
                usage("module2", 3) });
        when(dataStore.getUsages()).thenReturn(usages);
        List<Module> mostUsed = victim.getMostRecentlyUsed();
        assertEquals(2, mostUsed.size());
        assertEquals("module2", mostUsed.get(0).id());
    }

}
