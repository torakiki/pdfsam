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
package org.pdfsam.ui.banner;

import static org.pdfsam.ui.commons.SetActiveModuleRequest.activeteModule;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleCategory;

import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;

/**
 * Menu letting the user select the module he/she wants to use
 * 
 * @author Andrea Vacondio
 * 
 */
class ModulesMenu extends Menu {

    @Inject
    public ModulesMenu(List<Module> modules) {
        super(DefaultI18nContext.getInstance().i18n("_Modules"));
        setId("modulesMenu");
        Map<ModuleCategory, Menu> moduleSubmenus = new HashMap<>();
        for (final Module currentModule : modules) {
            ModuleCategory category = currentModule.descriptor().category;
            Menu currentMenu = moduleSubmenus.get(category);
            if (currentMenu == null) {
                currentMenu = new Menu(category.getDescription());
                moduleSubmenus.put(category, currentMenu);
            }
            MenuItem moduleMenu = new MenuItem(currentModule.descriptor().getName());
            moduleMenu.setOnAction(e -> eventStudio().broadcast(activeteModule(currentModule.id())));
            currentMenu.getItems().add(moduleMenu);
        }
        getItems().addAll(moduleSubmenus.values());
    }
}
