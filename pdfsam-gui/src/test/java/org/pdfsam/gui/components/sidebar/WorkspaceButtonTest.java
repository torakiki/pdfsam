package org.pdfsam.gui.components.sidebar;

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

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.gui.components.content.workspace.WorkspaceMenu;
import org.pdfsam.service.ui.RecentWorkspacesService;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxAssert;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.matcher.base.NodeMatchers;

import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
class WorkspaceButtonTest {
    private FxRobot robot;
    private WorkspaceButton victim;

    @Start
    public void start(Stage stage) {
        var service = mock(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(List.of("Chuck", "Norris"));
        this.victim = new WorkspaceButton(new WorkspaceMenu(service));
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void onClick() {
        robot.clickOn(victim);
        FxAssert.verifyThat("#workspaceMenu", NodeMatchers.isVisible());
    }

}