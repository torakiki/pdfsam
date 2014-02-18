/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.menu;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.ui.SetCurrentModuleRequest;
import org.pdfsam.ui.module.BaseTaskExecutionModule;

/**
 * Menu letting the user select the module he/she wants to use
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class ModulesMenu extends Menu {
    @Inject
    private List<BaseTaskExecutionModule> modules;

    ModulesMenu() {
        super(DefaultI18nContext.getInstance().i18n("_Modules"));
    }

    @PostConstruct
    void initModulesMenu() {
        Map<ModuleCategory, Menu> moduleSubmenus = new HashMap<>();
        for (final BaseTaskExecutionModule currentModule : modules) {
            ModuleCategory category = currentModule.descriptor().getCategory();
            Menu currentMenu = moduleSubmenus.get(category);
            if (currentMenu == null) {
                currentMenu = new Menu(category.getDescription());
                moduleSubmenus.put(category, currentMenu);
            }
            MenuItem moduleMenu = new MenuItem(currentModule.descriptor().getName());
            moduleMenu.setOnAction(e -> eventStudio().broadcast(new SetCurrentModuleRequest(currentModule.id())));
            currentMenu.getItems().add(moduleMenu);
        }
        getItems().addAll(moduleSubmenus.values());
    }
}
