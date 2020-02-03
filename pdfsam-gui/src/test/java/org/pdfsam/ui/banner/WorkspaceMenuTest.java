/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ago/2014
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
package org.pdfsam.ui.banner;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Arrays;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.pdfsam.NoHeadless;
import org.pdfsam.injector.Injector;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.RecentWorkspacesService;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.ui.workspace.WorkspaceLoadedEvent;
import org.sejda.eventstudio.Listener;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
@Category(NoHeadless.class)
public class WorkspaceMenuTest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private Injector injector;

    @Override
    public void start(Stage stage) {
        injector = Injector.start(new MenuConfig());
        Scene scene = new Scene(injector.instance(MenuButton.class));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void onRecentWorkspace() {
        Listener<LoadWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceEvent.class, listener);
        clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#recentWorkspace").clickOn("Chuck");
        verify(listener).onEvent(any());
    }

    @Test
    public void recentIsUpdated() {
        RecentWorkspacesService service = injector.instance(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(Arrays.asList("Michael"));
        eventStudio().broadcast(new WorkspaceLoadedEvent(mock(File.class)));
        clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#recentWorkspace").clickOn("Michael");
    }

    @Test
    public void recentIsUpdatedAndMnemonicAreNotParsed() {
        RecentWorkspacesService service = injector.instance(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(Arrays.asList("I_have_underscores"));
        eventStudio().broadcast(new WorkspaceLoadedEvent(mock(File.class)));
        clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#recentWorkspace").clickOn("I_have_underscores");
    }

    @Test
    public void recentIsCleared() {
        RecentWorkspacesService service = injector.instance(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(Arrays.asList("Michael"));
        eventStudio().broadcast(new WorkspaceLoadedEvent(mock(File.class)));
        assertTrue(clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#recentWorkspace").lookup("Michael").tryQuery().isPresent());
        clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#clearWorkspaces");
        assertFalse(clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#recentWorkspace").lookup("Michael").tryQuery().isPresent());
    }
}
