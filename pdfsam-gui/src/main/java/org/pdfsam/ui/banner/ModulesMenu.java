/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/nov/2013
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

import static org.pdfsam.ui.commons.SetActiveModuleRequest.activeteModule;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import org.pdfsam.i18n.I18nContext;
import org.pdfsam.module.Tool;
import org.pdfsam.module.ToolCategory;

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
    public ModulesMenu(List<Tool> tools) {
        super(i18n().tr("_Modules"));
        setId("modulesMenu");
        Map<ToolCategory, Menu> moduleSubmenus = new HashMap<>();
        for (final Tool currentTool : tools) {
            ToolCategory category = currentTool.descriptor().category;
            Menu currentMenu = moduleSubmenus.get(category);
            if (currentMenu == null) {
                currentMenu = new Menu(category.getDescription());
                moduleSubmenus.put(category, currentMenu);
            }
            MenuItem moduleMenu = new MenuItem(currentTool.descriptor().getName());
            moduleMenu.setOnAction(e -> eventStudio().broadcast(activeteModule(currentTool.id())));
            currentMenu.getItems().add(moduleMenu);
        }
        getItems().addAll(moduleSubmenus.values());
    }
}
