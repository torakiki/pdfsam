/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 31/ott/2013
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

import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.MenuItem;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.configuration.ApplicationContextHolder;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.about.AboutStage;
import org.pdfsam.gui.preference.PreferenceStage;
import org.pdfsam.ui.ShowStageHandler;
import org.pdfsam.ui.support.Style;

/**
 * {@link MenuBar} of the PDFsam application
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class AppMenuBar extends MenuBar {

    @Inject
    private PreferenceStage preferenceStage;
    @Inject
    private AboutStage aboutStage;
    @Inject
    private WorkspaceMenu workspace;
    @Inject
    private ModulesMenu modulesMenu;

    @PostConstruct
    private void initMenues() {
        getStyleClass().addAll(Style.MENU_BAR.css());
        Menu file = new Menu(DefaultI18nContext.getInstance().i18n("_File"));
        MenuItem exit = new MenuItem(DefaultI18nContext.getInstance().i18n("E_xit"));
        exit.setOnAction(e -> {
            ApplicationContextHolder.getContext().close();
            System.exit(0);
        });
        file.getItems().add(exit);

        Menu edit = new Menu(DefaultI18nContext.getInstance().i18n("_Edit"));
        MenuItem preferences = new MenuItem(DefaultI18nContext.getInstance().i18n("_Preferences"));
        preferences.setOnAction(new ShowStageHandler(preferenceStage));
        edit.getItems().add(preferences);

        Menu help = new Menu(DefaultI18nContext.getInstance().i18n("_Help"));
        MenuItem about = new MenuItem(DefaultI18nContext.getInstance().i18n("_About"));
        about.setOnAction(new ShowStageHandler(aboutStage));
        help.getItems().add(about);
        getMenus().addAll(file, edit, workspace, modulesMenu, help);
    }

}
