package org.pdfsam.gui.components.sidebar;

import jakarta.inject.Named;
import javafx.scene.Scene;
import javafx.scene.input.MouseButton;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Isolated;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.gui.components.content.about.AboutContentItem;
import org.pdfsam.gui.components.content.about.AboutPane;
import org.pdfsam.gui.components.content.home.HomeContentItem;
import org.pdfsam.gui.components.content.home.HomePane;
import org.pdfsam.gui.components.content.preference.PreferenceContentItem;
import org.pdfsam.gui.components.content.preference.PreferencePane;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Key;
import org.pdfsam.injector.Provides;
import org.pdfsam.model.lifecycle.ShutdownEvent;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.ShowErrorMessagesRequest;
import org.pdfsam.persistence.PreferencesRepository;
import org.pdfsam.service.ui.RecentWorkspacesService;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.HighPriorityTestTool;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@pdfsam.org).
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
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
@Isolated
class VerticalSidebarUITest {

    private FxRobot robot;
    private VerticalSidebar victim;
    private Injector injector;

    @BeforeAll
    public static void setUp() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
        app().persistentSettings().set(BooleanPersistentProperty.SIDEBAR_EXPANDED_STATE, true);
    }

    @Start
    public void start(Stage stage) {
        this.injector = Injector.start(new Config());
        this.victim = injector.instance(VerticalSidebar.class);
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void itemSelectedOnClick() {
        var logButton = injector.instance(LogButton.class);
        assertFalse(logButton.isSelected());
        robot.clickOn(logButton);
        assertTrue(logButton.isSelected());
    }

    @Test
    public void toolSelectedOnClick() {
        var toolButton = robot.lookup(".tool-selectable-sidebar-button").queryAs(ToolSelectableSidebarButton.class);
        assertFalse(toolButton.isSelected());
        robot.clickOn(toolButton);
        assertTrue(toolButton.isSelected());
    }

    @Test
    @DisplayName("Drag and drop on tools stores the new order")
    @Tag("NoHeadless")
    public void dragoAndDrop() {
        var repo = injector.instance(Key.of(PreferencesRepository.class, "toolsOrderRepository"));
        robot.drag("HighPriorityTestModule", MouseButton.PRIMARY).dropTo("TestModule");
        eventStudio().broadcast(new ShutdownEvent());
        verify(repo).saveInt("toolOrder_" + HighPriorityTestTool.ID, 0);
        verify(repo).saveInt("toolOrder_" + DefaultPriorityTestTool.ID, 1);
    }

    @Test
    @DisplayName("Drag and drop on non tools does not store the new order")
    @Tag("NoHeadless")
    public void dragoAndDropOnNonTools() {
        var logButton = injector.instance(LogButton.class);
        var repo = injector.instance(Key.of(PreferencesRepository.class, "toolsOrderRepository"));
        robot.drag("HighPriorityTestModule", MouseButton.PRIMARY).dropTo(logButton);
        eventStudio().broadcast(new ShutdownEvent());
        verify(repo, never()).saveInt(any(), anyInt());
    }

    @Test
    public void displayText() {
        var logButton = injector.instance(LogButton.class);
        assertTrue(logButton.displayTextProperty().get());
        robot.clickOn(".sidebar-expand-toggle");
        assertFalse(logButton.displayTextProperty().get());
    }

    @Test
    public void showLogs() {
        eventStudio().broadcast(new ShowErrorMessagesRequest());
        var logButton = injector.instance(LogButton.class);
        assertTrue(logButton.isSelected());
    }

    @Test
    public void buttonIsDisabledWhenSelected() {
        var logButton = injector.instance(LogButton.class);
        robot.clickOn(logButton);
        assertTrue(logButton.isSelected());
        assertTrue(logButton.getWrapped().isDisabled());
    }

    static class Config {

        @Provides
        public VerticalSidebar sidebar(HomeContentItem homeItem, LogButton logButton, NewsButton newsButton,
                PreferenceContentItem preferenceItem, AboutContentItem aboutItem, WorkspaceButton workspaceButton,
                ToolsButtons toolsButtons) {
            return new VerticalSidebar(homeItem, logButton, newsButton, preferenceItem, aboutItem, workspaceButton,
                    toolsButtons);
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
        public ToolsButtons toolsButtons(@Named("toolsOrderRepository") PreferencesRepository repo) {
            return new ToolsButtons(repo,
                    Map.of(DefaultPriorityTestTool.ID, new DefaultPriorityTestTool(), HighPriorityTestTool.ID,
                            new HighPriorityTestTool()));
        }

        @Provides
        @Named("toolsOrderRepository")
        PreferencesRepository toolsOrderRepo() {
            var repo = mock(PreferencesRepository.class);
            when(repo.getInt(any(), anyInt())).thenReturn(1);
            return repo;
        }

    }
}