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
package org.pdfsam.gui.components.content.workspace;

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.gui.components.sidebar.WorkspaceButton;
import org.pdfsam.injector.Injector;
import org.pdfsam.model.ui.workspace.LoadWorkspaceRequest;
import org.pdfsam.model.ui.workspace.WorkspaceLoadedEvent;
import org.pdfsam.service.ui.RecentWorkspacesService;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitTestListener;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class WorkspaceMenuTest {
    private Injector injector;
    private FxRobot robot;
    private Path workspace;

    @Start
    public void start(Stage stage) {
        injector = Injector.start(new MenuConfig());
        Scene scene = new Scene(injector.instance(WorkspaceButton.class));
        stage.setScene(scene);
        stage.show();
    }

    @BeforeEach
    public void setUp(@TempDir Path folder){
        this.workspace = folder.resolve("workspace.json");
    }

    @Test
    public void onRecentWorkspace() {
        HitTestListener<LoadWorkspaceRequest> listener = new HitTestListener<>();
        eventStudio().add(LoadWorkspaceRequest.class, listener);
        robot.clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#recentWorkspace").clickOn("Chuck");
        assertTrue(listener.isHit());
    }

    @Test
    public void recentIsUpdated() {
        RecentWorkspacesService service = injector.instance(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(List.of("Michael"));
        eventStudio().broadcast(new WorkspaceLoadedEvent(workspace.toFile()));
        robot.clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#recentWorkspace").clickOn("Michael");
    }

    @Test
    public void recentIsUpdatedAndMnemonicAreNotParsed() {
        RecentWorkspacesService service = injector.instance(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(List.of("I_have_underscores"));
        eventStudio().broadcast(new WorkspaceLoadedEvent(workspace.toFile()));
        robot.clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#recentWorkspace").clickOn("I_have_underscores");
    }

    @Test
    public void recentIsCleared() {
        RecentWorkspacesService service = injector.instance(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(List.of("Michael"));
        eventStudio().broadcast(new WorkspaceLoadedEvent(workspace.toFile()));
        assertTrue(robot.clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#recentWorkspace").lookup("Michael").tryQuery().isPresent());
        robot.clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#clearWorkspaces");
        assertFalse(robot.clickOn(".button").clickOn("#workspaceMenu").moveTo("#loadWorkspace").moveTo("#saveWorkspace")
                .clickOn("#recentWorkspace").lookup("Michael").tryQuery().isPresent());
    }
}
