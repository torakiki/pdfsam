package org.pdfsam.gui;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.ApplicationRuntimeState;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.gui.components.content.about.AboutContentItem;
import org.pdfsam.gui.components.content.about.AboutPane;
import org.pdfsam.gui.components.content.home.HomeContentItem;
import org.pdfsam.gui.components.content.home.HomePane;
import org.pdfsam.gui.components.content.news.NewsPanel;
import org.pdfsam.gui.components.content.preference.PreferenceContentItem;
import org.pdfsam.gui.components.content.preference.PreferencePane;
import org.pdfsam.gui.components.sidebar.LogButton;
import org.pdfsam.gui.components.sidebar.NewsButton;
import org.pdfsam.gui.components.sidebar.ToolsButtons;
import org.pdfsam.gui.components.sidebar.VerticalSidebar;
import org.pdfsam.gui.components.sidebar.WorkspaceButton;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.ContentItem;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.model.ui.SetTitleRequest;
import org.pdfsam.model.ui.ShowLogMessagesRequest;
import org.pdfsam.persistence.PreferencesRepository;
import org.pdfsam.service.ui.RecentWorkspacesService;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.JavaFxThreadInitializeExtension;

import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
@ExtendWith({ ClearEventStudioExtension.class, JavaFxThreadInitializeExtension.class })
class AppContentControllerTest {

    private AppContentController victim;

    private Injector injector;
    private ApplicationRuntimeState runtimeState;

    @BeforeEach
    public void setUp() {
        injector = Injector.start(new AppContentControllerTest.Config());
        var appcontainer = injector.instance(AppContainer.class);
        var news = injector.instance(NewsPanel.class);
        var itemsList = injector.instancesOfType(ContentItem.class);
        this.runtimeState = mock(ApplicationRuntimeState.class);
        var persistentSettings = mock(ApplicationPersistentSettings.class);
        var context = mock(ApplicationContext.class);
        when(context.runtimeState()).thenReturn(runtimeState);
        when(context.persistentSettings()).thenReturn(persistentSettings);
        when(runtimeState.tools()).thenReturn(Map.of(DefaultPriorityTestTool.ID, new DefaultPriorityTestTool()));
        this.victim = new AppContentController(appcontainer, news, itemsList, context);
    }

    static class Config {
        @Provides
        public VerticalSidebar sidebar(HomeContentItem homeItem, LogButton logButton, NewsButton newsButton,
                PreferenceContentItem preferenceItem, AboutContentItem aboutItem, WorkspaceButton workspaceButton,
                ToolsButtons tools) {
            return new VerticalSidebar(homeItem, logButton, newsButton, preferenceItem, aboutItem, workspaceButton,
                    tools);
        }

        @Provides
        public HomePane homePanel(List<Tool> tools) {
            return new HomePane(tools);
        }

        @Provides
        public PreferencePane preferencesPane() {
            return mock(PreferencePane.class);
        }

        @Provides
        public AboutPane aboutPane() {
            return mock(AboutPane.class);
        }

        @Provides
        public RecentWorkspacesService service() {
            RecentWorkspacesService service = mock(RecentWorkspacesService.class);
            when(service.getRecentlyUsedWorkspaces()).thenReturn(List.of("Chuck", "Norris"));
            return service;
        }

        @Provides
        public DefaultPriorityTestTool tool() {
            return new DefaultPriorityTestTool();
        }

        @Provides
        public ToolsButtons toolsButtons() {
            var repo = mock(PreferencesRepository.class);
            when(repo.getInt(any(), anyInt())).thenReturn(1);
            return new ToolsButtons(repo, Map.of(DefaultPriorityTestTool.ID, new DefaultPriorityTestTool()));
        }
    }

    @Test
    void onSetActiveContentItem() {
        Listener<SetTitleRequest> listener = mock(Listener.class);
        eventStudio().add(SetTitleRequest.class, listener);
        victim.onSetActiveContentItem(new SetActiveContentItemRequest(DefaultPriorityTestTool.ID));
        verify(listener).onEvent(any(SetTitleRequest.class));
        verify(runtimeState).activeTool(any(DefaultPriorityTestTool.class));
    }

    @Test
    void onSetActiveContentItemNoTool() {
        Listener<SetTitleRequest> listener = mock(Listener.class);
        eventStudio().add(SetTitleRequest.class, listener);
        victim.onSetActiveContentItem(new SetActiveContentItemRequest("Chuck Norris"));
        verify(runtimeState).activeTool(isNull());
    }

    @Test
    void onShowErrorMessagesRequest() {
        Listener<SetTitleRequest> listener = mock(Listener.class);
        eventStudio().add(SetTitleRequest.class, listener);
        victim.onShowLogMessagesRequest(new ShowLogMessagesRequest());
        verify(listener).onEvent(any(SetTitleRequest.class));
        verify(runtimeState, never()).activeTool(any());
    }

}