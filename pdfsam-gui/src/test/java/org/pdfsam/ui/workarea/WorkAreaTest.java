/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/ago/2014
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
package org.pdfsam.ui.workarea;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.List;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.pdfsam.module.Module;
import org.pdfsam.module.UsageService;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestModule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.commons.SetActiveModuleRequest;
import org.pdfsam.ui.event.SetTitleEvent;
import org.sejda.eventstudio.Listener;
import org.sejda.injector.Injector;
import org.sejda.injector.Provides;

import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;

/**
 * @author Andrea Vacondio
 *
 */
public class WorkAreaTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    private Injector injector;

    @Before
    public void setUp() {
        injector = Injector.start(new Config());
    }

    static class Config {

        @Provides
        public Module module() {
            return new TestModule();
        }

        @Provides
        public UsageService service() {
            return mock(UsageService.class);
        }

        @Provides
        public QuickbarModuleButtonsProvider provider(UsageService service, List<Module> modules) {
            return new QuickbarModuleButtonsProvider(service, modules);
        }

        @Provides
        public QuickbarModuleButtonsPane buttons(QuickbarModuleButtonsProvider provider) {
            return new QuickbarModuleButtonsPane(provider);
        }

        @Provides
        public WorkArea victim(List<Module> modules, QuickbarModuleButtonsPane pane) {
            return new WorkArea(modules, pane);
        }
    }

    @Test
    public void wrongModuleDoesntBoom() {
        WorkArea victim = injector.instance(WorkArea.class);
        victim.onSetActiveModule(SetActiveModuleRequest.activeteModule("chuck norris"));
    }

    @Test
    public void eventTitleIsSent() {
        WorkArea victim = injector.instance(WorkArea.class);
        assertNull(victim.lookup("#modulePane"));
        Listener<SetTitleEvent> listener = mock(Listener.class);
        eventStudio().add(SetTitleEvent.class, listener);
        victim.onSetActiveModule(SetActiveModuleRequest.activeteModule(DefaultPriorityTestModule.ID));
        ArgumentCaptor<SetTitleEvent> captor = ArgumentCaptor.forClass(SetTitleEvent.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(injector.instance(TestModule.class).descriptor().getName(), captor.getValue().getTitle());
        assertNotNull(victim.lookup("#modulePane"));
    }

    @Test
    public void emptyEventTitleIsSent() {
        WorkArea victim = injector.instance(WorkArea.class);
        Listener<SetTitleEvent> listener = mock(Listener.class);
        eventStudio().add(SetTitleEvent.class, listener);
        victim.onSetActiveModule(SetActiveModuleRequest.activeteCurrentModule());
        ArgumentCaptor<SetTitleEvent> captor = ArgumentCaptor.forClass(SetTitleEvent.class);
        verify(listener).onEvent(captor.capture());
        assertTrue(isBlank(captor.getValue().getTitle()));
    }

    @Test
    public void previousEventTitleIsSent() {
        WorkArea victim = injector.instance(WorkArea.class);
        victim.onSetActiveModule(SetActiveModuleRequest.activeteModule(DefaultPriorityTestModule.ID));
        eventStudio().clear();
        Listener<SetTitleEvent> listener = mock(Listener.class);
        eventStudio().add(SetTitleEvent.class, listener);
        victim.onSetActiveModule(SetActiveModuleRequest.activeteCurrentModule());
        ArgumentCaptor<SetTitleEvent> captor = ArgumentCaptor.forClass(SetTitleEvent.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(injector.instance(TestModule.class).descriptor().getName(), captor.getValue().getTitle());
    }

    public static class TestModule extends DefaultPriorityTestModule {
        @Override
        public Pane modulePanel() {
            HBox panel = new HBox();
            panel.setId("modulePane");
            return panel;
        }
    }
}
