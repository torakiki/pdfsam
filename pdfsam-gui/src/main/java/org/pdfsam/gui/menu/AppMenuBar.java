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

import org.bushe.swing.event.EventBus;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.ShowStageHandler;
import org.pdfsam.gui.about.AboutStage;
import org.pdfsam.gui.preference.PreferenceStage;
import org.pdfsam.gui.workspace.LoadWorkspaceEvent;
import org.pdfsam.gui.workspace.SaveWorkspaceEvent;
import org.pdfsam.module.BaseTaskExecutionModule;
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
    private Map<String, BaseTaskExecutionModule> modulesMap;
    @Inject
    private PreferenceStage preferenceStage;
    @Inject
    private AboutStage aboutStage;

    @PostConstruct
    private void initModues() {
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
                EventBus.publish(new LoadWorkspaceEvent());
            }
        });
        MenuItem save = new MenuItem(DefaultI18nContext.getInstance().i18n("_Save"));
        save.setAccelerator(new KeyCodeCombination(KeyCode.S, KeyCombination.SHORTCUT_DOWN));
        save.setOnAction(new EventHandler<ActionEvent>() {
            public void handle(ActionEvent event) {
                EventBus.publish(new SaveWorkspaceEvent());
            }
        });
        Menu recent = new Menu(DefaultI18nContext.getInstance().i18n("_Recent"));
        workspace.getItems().addAll(load, save, new SeparatorMenuItem(), recent);

        Menu modules = new Menu(DefaultI18nContext.getInstance().i18n("_Modules"));
        Menu help = new Menu(DefaultI18nContext.getInstance().i18n("_Help"));
        MenuItem about = new MenuItem(DefaultI18nContext.getInstance().i18n("_About"));
        about.setOnAction(new ShowStageHandler(aboutStage));
        help.getItems().add(about);
        getMenus().addAll(file, edit, workspace, modules, help);
        // do something to show modules
        System.out.println(modulesMap);
    }

    /**
     * Handler for the exit action
     * 
     * @author Andrea Vacondio
     */
    private static class ExitActionHandler implements EventHandler<ActionEvent> {

        public void handle(ActionEvent event) {
            // TODO maybe send an event and perform needed actions before exiting
            System.exit(0);
        }
    }
}
