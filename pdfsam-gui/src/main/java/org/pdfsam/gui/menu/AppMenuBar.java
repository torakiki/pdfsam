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

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.configuration.ApplicationContextHolder;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.SetCurrentModuleRequest;
import org.pdfsam.gui.about.AboutStage;
import org.pdfsam.gui.preference.PreferenceStage;
import org.pdfsam.gui.workspace.LoadWorkspaceEvent;
import org.pdfsam.gui.workspace.SaveWorkspaceEvent;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.ui.ShowStageHandler;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
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
    private List<BaseTaskExecutionModule> modules;
    @Inject
    private PreferenceStage preferenceStage;
    @Inject
    private AboutStage aboutStage;

    @PostConstruct
    private void initMenues() {
        getStyleClass().addAll(Style.MENU_BAR.css());
        Menu file = new Menu(DefaultI18nContext.getInstance().i18n("_File"));
        MenuItem exit = new MenuItem(DefaultI18nContext.getInstance().i18n("E_xit"));
        exit.setOnAction(new ExitActionHandler());
        file.getItems().add(exit);

        Menu edit = new Menu(DefaultI18nContext.getInstance().i18n("_Edit"));
        MenuItem preferences = new MenuItem(DefaultI18nContext.getInstance().i18n("_Preferences"));
        preferences.setOnAction(new ShowStageHandler(preferenceStage));
        edit.getItems().add(preferences);

        Menu workspace = new Menu(DefaultI18nContext.getInstance().i18n("_Workspace"));
        MenuItem load = new MenuItem(DefaultI18nContext.getInstance().i18n("_Load"));
        load.setAccelerator(new KeyCodeCombination(KeyCode.L, KeyCombination.SHORTCUT_DOWN));
        load.setOnAction(new EventHandler<ActionEvent>() {
            public void handle(ActionEvent event) {
                eventStudio().broadcast(new LoadWorkspaceEvent());
            }
        });
        MenuItem save = new MenuItem(DefaultI18nContext.getInstance().i18n("_Save"));
        save.setAccelerator(new KeyCodeCombination(KeyCode.S, KeyCombination.SHORTCUT_DOWN));
        save.setOnAction(new EventHandler<ActionEvent>() {
            public void handle(ActionEvent event) {
                eventStudio().broadcast(new SaveWorkspaceEvent());
            }
        });
        Menu recent = new Menu(DefaultI18nContext.getInstance().i18n("_Recent"));
        workspace.getItems().addAll(load, save, new SeparatorMenuItem(), recent);

        Menu modulesMenu = new Menu(DefaultI18nContext.getInstance().i18n("_Modules"));
        initModulesMenu(modulesMenu);

        Menu help = new Menu(DefaultI18nContext.getInstance().i18n("_Help"));
        MenuItem about = new MenuItem(DefaultI18nContext.getInstance().i18n("_About"));
        about.setOnAction(new ShowStageHandler(aboutStage));
        help.getItems().add(about);
        getMenus().addAll(file, edit, workspace, modulesMenu, help);
    }

    private void initModulesMenu(Menu modulesMenu) {
        Map<ModuleCategory, Menu> moduleSubmenus = new HashMap<>();
        for (final BaseTaskExecutionModule currentModule : modules) {
            ModuleCategory category = currentModule.descriptor().getCategory();
            Menu currentMenu = moduleSubmenus.get(category);
            if (currentMenu == null) {
                currentMenu = new Menu(category.getDescription());
                moduleSubmenus.put(category, currentMenu);
            }
            MenuItem moduleMenu = new MenuItem(currentModule.descriptor().getName());
            moduleMenu.setOnAction(new EventHandler<ActionEvent>() {
                public void handle(ActionEvent event) {
                    eventStudio().broadcast(new SetCurrentModuleRequest(currentModule.id()));
                }
            });
            currentMenu.getItems().add(moduleMenu);
        }
        modulesMenu.getItems().addAll(moduleSubmenus.values());
    }

    /**
     * Handler for the exit action
     * 
     * @author Andrea Vacondio
     */
    private static class ExitActionHandler implements EventHandler<ActionEvent> {

        public void handle(ActionEvent event) {
            ApplicationContextHolder.getContext().close();
            System.exit(0);
        }
    }
}
