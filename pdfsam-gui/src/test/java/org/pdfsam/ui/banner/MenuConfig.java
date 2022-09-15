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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.pdfsam.injector.Prototype;
import org.pdfsam.injector.Provides;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.ui.RecentWorkspacesService;

/**
 * @author Andrea Vacondio
 *
 */
public class MenuConfig {
    @Provides
    @Prototype
    public MenuButton menuButton(AppContextMenu menu) {
        return new MenuButton(menu);
    }

    @Provides
    public AppContextMenu menu(WorkspaceMenu workspaceMenu, ModulesMenu modulesMenu) {
        return spy(new AppContextMenu(workspaceMenu, modulesMenu));
    }

    @Provides
    public WorkspaceMenu workspaceMenu(RecentWorkspacesService service) {
        return new WorkspaceMenu(service);
    }

    @Provides
    public ModulesMenu modulesMenu() {
        return new ModulesMenu(Arrays.asList(new DefaultPriorityTestTool()));
    }

    @Provides
    public DefaultPriorityTestTool module() {
        return new DefaultPriorityTestTool();
    }

    @Provides
    public RecentWorkspacesService service() {
        RecentWorkspacesService service = mock(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(Arrays.asList("Chuck", "Norris"));
        return service;
    }
}
