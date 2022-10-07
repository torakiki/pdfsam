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
package org.pdfsam.gui.components.banner;

import jakarta.inject.Inject;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolCategory;
import org.pdfsam.model.ui.SetActiveToolRequest;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Menu letting the user select the module he/she wants to use
 *
 * @author Andrea Vacondio
 */
class ToolsMenu extends Menu {

    @Inject
    public ToolsMenu() {
        this(app().runtimeState().tools().values());
    }

    public ToolsMenu(Collection<Tool> tools) {
        super(i18n().tr("_Tools"));
        setId("toolsMenu");
        Map<ToolCategory, Menu> moduleSubmenus = new HashMap<>();
        for (Tool tool : tools) {
            var categoryMenu = moduleSubmenus.computeIfAbsent(tool.descriptor().category(),
                    category -> new Menu(category.getDescription()));
            MenuItem moduleMenu = new MenuItem(tool.descriptor().name());
            moduleMenu.setOnAction(e -> eventStudio().broadcast(new SetActiveToolRequest(tool.id())));
            categoryMenu.getItems().add(moduleMenu);
        }
        getItems().addAll(moduleSubmenus.values());
    }
}
