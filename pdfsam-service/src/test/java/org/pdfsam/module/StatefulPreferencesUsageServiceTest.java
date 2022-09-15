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

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.test.ClearEventStudioRule;

import javax.inject.Named;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.module.ToolDescriptorBuilder.builder;

/**
 * @author Andrea Vacondio
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
        public Tool module1() {
            Tool tool = mock(Tool.class);
            when(tool.id()).thenReturn("module1");
            ToolDescriptor descriptor = builder().name("module1").description("desc").category(ToolCategory.SPLIT)
                    .priority(ToolPriority.DEFAULT)
                    .build();
            when(tool.descriptor()).thenReturn(descriptor);
            return tool;
        }

        @Provides
        @Named("module2")
        public Tool module2() {
            Tool tool = mock(Tool.class);
            when(tool.id()).thenReturn("module2");
            ToolDescriptor descriptor = builder().name("module2").description("desc").category(ToolCategory.MERGE)
                    .priority(ToolPriority.HIGH)
                    .build();
            when(tool.descriptor()).thenReturn(descriptor);
            return tool;
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
}
