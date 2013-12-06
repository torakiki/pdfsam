/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.menu;

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;

import javax.annotation.PostConstruct;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.workspace.LoadWorkspaceEvent;
import org.pdfsam.gui.workspace.SaveWorkspaceEvent;

/**
 * Menu displaying workspace related items
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class WorkspaceMenu extends Menu {

    public WorkspaceMenu() {
        super(DefaultI18nContext.getInstance().i18n("_Workspace"));
    }

    @PostConstruct
    void init() {
        MenuItem load = new MenuItem(DefaultI18nContext.getInstance().i18n("_Load"));
        load.setOnAction(e -> eventStudio().broadcast(new LoadWorkspaceEvent()));
        MenuItem save = new MenuItem(DefaultI18nContext.getInstance().i18n("_Save"));
        save.setOnAction(e -> eventStudio().broadcast(new SaveWorkspaceEvent()));
        Menu recent = new Menu(DefaultI18nContext.getInstance().i18n("Recen_t"));
        getItems().addAll(load, save, new SeparatorMenuItem(), recent);
    }
}
