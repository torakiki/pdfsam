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

import static org.mockito.Mockito.spy;

import org.pdfsam.module.Module;
import org.pdfsam.test.TestModule;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Scope;

/**
 * @author Andrea Vacondio
 *
 */
@Configuration
public class MenuConfig {
    @Bean
    @Lazy
    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public MenuButton menuButton() {
        return new MenuButton();
    }

    @Bean
    @Lazy
    public AppContextMenu menu() {
        return spy(new AppContextMenu());
    }

    @Bean
    @Lazy
    public WorkspaceMenu workspaceMenu() {
        return new WorkspaceMenu();
    }

    @Bean
    @Lazy
    public ModulesMenu modulesMenu() {
        return new ModulesMenu();
    }

    @Bean
    public Module module() {
        return new TestModule();
    }
}
