/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mag/2014
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

import javafx.application.Platform;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.configuration.ApplicationContextHolder;
import org.pdfsam.context.DefaultI18nContext;

/**
 * Context menu showing app functionalities. This is supposed to be activated by the menu button
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class AppContextMenu extends ContextMenu {
    @Inject
    private WorkspaceMenu workspace;
    @Inject
    private ModulesMenu modulesMenu;

    @PostConstruct
    private void initMenues() {
        MenuItem exit = new MenuItem(DefaultI18nContext.getInstance().i18n("E_xit"));
        exit.setOnAction(e -> {
            ApplicationContextHolder.getContext().close();
            Platform.exit();
        });

        getItems().addAll(workspace, modulesMenu, new SeparatorMenuItem(), exit);
    }
}
