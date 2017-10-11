/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ago/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Arrays;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.RecentWorkspacesService;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.ui.workspace.WorkspaceLoadedEvent;
import org.sejda.eventstudio.Listener;
import org.sejda.injector.Injector;

import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class WorkspaceMenuTest extends GuiTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private Injector injector;

    @Override
    protected Parent getRootNode() {
        injector = Injector.start(new MenuConfig());
        return injector.instance(MenuButton.class);
    }

    @Test
    public void onRecentWorkspace() {
        Listener<LoadWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceEvent.class, listener);
        click(".button").click("#workspaceMenu").move("#loadWorkspace").move("#saveWorkspace")
                .click("#recentWorkspace").click("Chuck");
        verify(listener).onEvent(any());
    }

    @Test
    public void recentIsUpdated() {
        RecentWorkspacesService service = injector.instance(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(Arrays.asList("Micheal"));
        eventStudio().broadcast(new WorkspaceLoadedEvent(mock(File.class)));
        click(".button").click("#workspaceMenu").move("#loadWorkspace").move("#saveWorkspace")
                .click("#recentWorkspace").click("Micheal");
    }

    @Test
    public void recentIsUpdatedAndMnemonicAreNotParsed() {
        RecentWorkspacesService service = injector.instance(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(Arrays.asList("I_have_underscores"));
        eventStudio().broadcast(new WorkspaceLoadedEvent(mock(File.class)));
        click(".button").click("#workspaceMenu").move("#loadWorkspace").move("#saveWorkspace").click("#recentWorkspace")
                .click("I_have_underscores");
    }
}
