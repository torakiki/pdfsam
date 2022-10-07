/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28/ago/2014
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
package org.pdfsam.gui.components.workarea;

import javafx.scene.control.ScrollPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.SetActiveToolRequest;
import org.pdfsam.model.ui.SetTitleRequest;
import org.pdfsam.service.tool.UsageService;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.JavaFxThreadExtension;
import org.pdfsam.ui.components.tool.RunButtonTriggerRequest;

import java.util.Map;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class, ClearEventStudioExtension.class })
public class WorkAreaTest {

    private Injector injector;

    @BeforeEach
    public void setUp() {
        injector = Injector.start(new Config());
    }

    static class Config {

        @Provides
        public UsageService service() {
            return mock(UsageService.class);
        }

        @Provides
        public Map<String, Tool> tools() {
            var tool = new TestTool();
            return Map.of(tool.id(), tool);
        }

        @Provides
        public QuickbarToolButtonsPane buttons(Map<String, Tool> tools) {
            return new QuickbarToolButtonsPane(tools.values());
        }

        @Provides
        public WorkArea victim(Map<String, Tool> tools, QuickbarToolButtonsPane pane) {
            return new WorkArea(pane, tools);
        }
    }

    @Test
    public void wrongModuleDoesntBoom() {
        WorkArea victim = injector.instance(WorkArea.class);
        victim.onSetActiveModule(new SetActiveToolRequest("chuck norris"));
    }

    @Test
    public void eventTitleIsSent() {
        WorkArea victim = injector.instance(WorkArea.class);
        assertNull(((ScrollPane) victim.getCenter()).getContent());
        Listener<SetTitleRequest> listener = mock(Listener.class);
        eventStudio().add(SetTitleRequest.class, listener);
        victim.onSetActiveModule(new SetActiveToolRequest(DefaultPriorityTestTool.ID));
        ArgumentCaptor<SetTitleRequest> captor = ArgumentCaptor.forClass(SetTitleRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(injector.instance(TestTool.class).descriptor().name(), captor.getValue().title());
        assertNotNull(((ScrollPane) victim.getCenter()).getContent());
    }

    @Test
    public void runRequestIsSent() {
        WorkArea victim = injector.instance(WorkArea.class);
        assertNull(((ScrollPane) victim.getCenter()).getContent());
        victim.onSetActiveModule(new SetActiveToolRequest(DefaultPriorityTestTool.ID));
        victim.setVisible(true);
        Listener<RunButtonTriggerRequest> listener = mock(Listener.class);
        eventStudio().add(RunButtonTriggerRequest.class, listener, DefaultPriorityTestTool.ID);
        victim.onRunButtonAccelerator(RunButtonTriggerRequest.INSTANCE);
        verify(listener).onEvent(ArgumentMatchers.any());
    }

    @Test
    public void runRequestIsNotSentIfNotVisible() {
        WorkArea victim = injector.instance(WorkArea.class);
        assertNull(((ScrollPane) victim.getCenter()).getContent());
        victim.onSetActiveModule(new SetActiveToolRequest(DefaultPriorityTestTool.ID));
        victim.setVisible(false);
        Listener<RunButtonTriggerRequest> listener = mock(Listener.class);
        eventStudio().add(RunButtonTriggerRequest.class, listener, DefaultPriorityTestTool.ID);
        victim.onRunButtonAccelerator(RunButtonTriggerRequest.INSTANCE);
        verify(listener, never()).onEvent(ArgumentMatchers.any());
    }

    @Test
    public void emptyEventTitleIsSent() {
        WorkArea victim = injector.instance(WorkArea.class);
        Listener<SetTitleRequest> listener = mock(Listener.class);
        eventStudio().add(SetTitleRequest.class, listener);
        victim.onSetActiveModule(new SetActiveToolRequest("don't exist"));
        ArgumentCaptor<SetTitleRequest> captor = ArgumentCaptor.forClass(SetTitleRequest.class);
        verify(listener).onEvent(captor.capture());
        assertTrue(isBlank(captor.getValue().title()));
    }

    @Test
    public void previousEventTitleIsSent() {
        WorkArea victim = injector.instance(WorkArea.class);
        victim.onSetActiveModule(new SetActiveToolRequest(DefaultPriorityTestTool.ID));
        eventStudio().clear();
        Listener<SetTitleRequest> listener = mock(Listener.class);
        eventStudio().add(SetTitleRequest.class, listener);
        victim.onSetActiveModule(new SetActiveToolRequest("don't exist"));
        ArgumentCaptor<SetTitleRequest> captor = ArgumentCaptor.forClass(SetTitleRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(injector.instance(TestTool.class).descriptor().name(), captor.getValue().title());
    }

    public static class TestTool extends DefaultPriorityTestTool {
        @Override
        public Pane panel() {
            HBox panel = new HBox();
            panel.setId("modulePane");
            return panel;
        }
    }
}
