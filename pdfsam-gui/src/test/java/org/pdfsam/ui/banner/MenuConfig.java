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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import javax.inject.Scope;

import org.pdfsam.test.DefaultPriorityTestModule;
import org.pdfsam.ui.RecentWorkspacesService;

/**
 * @author Andrea Vacondio
 *
 */
@Configuration
@Lazy
public class MenuConfig {
    @Bean
    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public MenuButton menuButton() {
        return new MenuButton(menu());
    }

    @Bean
    public AppContextMenu menu() {
        return spy(new AppContextMenu(workspaceMenu(), modulesMenu()));
    }

    @Bean
    public WorkspaceMenu workspaceMenu() {
        return new WorkspaceMenu(service());
    }

    @Bean
    public ModulesMenu modulesMenu() {
        return new ModulesMenu(Arrays.asList(new DefaultPriorityTestModule()));
    }

    @Bean
    public DefaultPriorityTestModule module() {
        return new DefaultPriorityTestModule();
    }

    @Bean
    public RecentWorkspacesService service() {
        RecentWorkspacesService service = mock(RecentWorkspacesService.class);
        when(service.getRecentlyUsedWorkspaces()).thenReturn(Arrays.asList("Chuck", "Norris"));
        return service;
    }
}
