/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ago/2014
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
package org.pdfsam.ui.banner;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Arrays;

import javax.inject.Inject;

import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.runner.RunWith;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.RecentWorkspacesService;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.pdfsam.ui.workspace.WorkspaceLoadedEvent;
import org.sejda.eventstudio.Listener;

import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = { MenuConfig.class })
@DirtiesContext(classMode = ClassMode.AFTER_CLASS)
public class WorkspaceMenuTest extends GuiTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    @Inject
    private ApplicationContext applicationContext;

    @Override
    protected Parent getRootNode() {
        return applicationContext.getBean(MenuButton.class);
    }

    @Test
    @Ignore("need to find a way to test file chooser")
    public void onSaveClick() {
        Listener<SaveWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(SaveWorkspaceEvent.class, listener);
        click(".button").click("#workspaceMenu").move("#loadWorkspace")
                .click("#saveWorkspace");
        verify(listener).onEvent(any());
    }

    @Test
    @Ignore("need to find a way to test file chooser")
    public void onLoadClick() {
        Listener<LoadWorkspaceEvent> listener = mock(Listener.class);
        eventStudio().add(LoadWorkspaceEvent.class, listener);
        click(".button").click("#workspaceMenu").click("#loadWorkspace");
        verify(listener).onEvent(any());
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
    @DirtiesContext
    public void recentIsUpdated() {
        RecentWorkspacesService service = applicationContext.getBean(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(Arrays.asList("Micheal"));
        eventStudio().broadcast(new WorkspaceLoadedEvent(mock(File.class)));
        click(".button").click("#workspaceMenu").move("#loadWorkspace").move("#saveWorkspace")
                .click("#recentWorkspace").click("Micheal");
    }

    @Test
    @DirtiesContext
    public void recentIsUpdatedAndMnemonicAreNotParsed() {
        RecentWorkspacesService service = applicationContext.getBean(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(Arrays.asList("I_have_underscores"));
        eventStudio().broadcast(new WorkspaceLoadedEvent(mock(File.class)));
        click(".button").click("#workspaceMenu").move("#loadWorkspace").move("#saveWorkspace").click("#recentWorkspace")
                .click("I_have_underscores");
    }
}
