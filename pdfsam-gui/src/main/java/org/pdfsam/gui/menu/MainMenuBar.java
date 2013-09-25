/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/apr/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.HashMap;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.JDialog;
import javax.swing.JMenu;
import javax.swing.JMenuBar;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.configuration.ApplicationContextHolder;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.about.AboutDialog;
import org.pdfsam.gui.preference.PreferencesDialog;
import org.pdfsam.gui.support.SwingUtils;
import org.pdfsam.gui.workspace.LoadWorkspaceAction;
import org.pdfsam.gui.workspace.SaveWorkspaceAction;
import org.pdfsam.module.BaseTaskExecutionModule;
import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.TaskExecutionModulesLoadedEvent;

import bibliothek.gui.dock.common.CControl;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Menu bar for the main frame
 * 
 * @author Andrea Vacondio
 * 
 */
public class MainMenuBar extends JMenuBar {

    private Map<MenuType, JMenu> menus = new HashMap<>();
    private CControl control;

    public MainMenuBar(CControl control) {
        this.control = control;
        JMenu menuFile = new JMenu();
        menuFile.setText(DefaultI18nContext.getInstance().i18n("File"));
        menuFile.setMnemonic(KeyEvent.VK_F);
        menuFile.add(new ExitAction());
        add(menuFile);

        JMenu menuEdit = new JMenu();
        menuEdit.setText(DefaultI18nContext.getInstance().i18n("Edit"));
        menuEdit.setMnemonic(KeyEvent.VK_E);
        menuEdit.add(new ShowDialogAction(DefaultI18nContext.getInstance().i18n("Preferences"),
                ApplicationContextHolder.getContext().getBean(PreferencesDialog.class)));
        add(menuEdit);

        JMenu menuWorkspace = new JMenu();
        menuWorkspace.setText(DefaultI18nContext.getInstance().i18n("Workspace"));
        menuWorkspace.setMnemonic(KeyEvent.VK_W);
        menuWorkspace.add(new LoadWorkspaceAction());
        menuWorkspace.add(new SaveWorkspaceAction());
        menuWorkspace.addSeparator();
        JMenu recentWorkspaces = new JMenu();
        recentWorkspaces.setText(DefaultI18nContext.getInstance().i18n("Recent"));
        menuWorkspace.add(recentWorkspaces);
        add(menuWorkspace);

        JMenu menuModules = new JMenu();
        menuModules.setText(DefaultI18nContext.getInstance().i18n("Modules"));
        menuModules.setMnemonic(KeyEvent.VK_M);
        add(menuModules);

        JMenu menuHelp = new JMenu();
        menuHelp.setText(DefaultI18nContext.getInstance().i18n("Help"));
        menuHelp.setMnemonic(KeyEvent.VK_H);
        menuHelp.add(new ShowDialogAction(DefaultI18nContext.getInstance().i18n("About"), ApplicationContextHolder
                .getContext().getBean(AboutDialog.class)));
        add(menuHelp);

        menus.put(MenuType.FILE, menuFile);
        menus.put(MenuType.HELP, menuHelp);
        menus.put(MenuType.WORKSPACE, menuWorkspace);
        menus.put(MenuType.EDIT, menuEdit);
        menus.put(MenuType.MODULES, menuModules);
        AnnotationProcessor.process(this);
    }

    /**
     * Adds a menuitem to the Help menu to show or hide a system content panel.
     * 
     * @param type
     *            the menu type
     * @param panel
     */
    public void addSystemContentAction(MenuType type, Module module) {
        JMenu menu = menus.get(type);
        requireNotNull(menu, "Unable to fine the given menu: " + type);
        menu.add(new SystemContentAction(control, module));
    }

    @EventSubscriber
    public void initModulesMenu(TaskExecutionModulesLoadedEvent event) {
        Map<ModuleCategory, JMenu> moduleSubmenus = new HashMap<>();
        for (BaseTaskExecutionModule currentModule : event.getModules()) {
            ModuleCategory category = currentModule.getDescriptor().getCategory();
            JMenu currentMenu = moduleSubmenus.get(category);
            if (currentMenu == null) {
                currentMenu = new JMenu();
                currentMenu.setText(category.getDescription());
                moduleSubmenus.put(category, currentMenu);
                menus.get(MenuType.MODULES).add(currentMenu);
            }
            currentMenu.add(new SystemContentAction(control, currentModule));
        }

    }

    /**
     * Action used to display dialogs
     * 
     * @author Andrea Vacondio
     * 
     */
    private class ShowDialogAction extends AbstractAction {

        private JDialog dialog = null;

        ShowDialogAction(String menuText, JDialog dialog) {
            super(menuText);
            requireNotNull(dialog, "Input dialog cannot be null");
            this.dialog = dialog;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!dialog.isVisible()) {
                SwingUtils.centrePositionOnScreen(dialog);
                dialog.setVisible(true);
            }
            dialog.toFront();
        }
    }

}
