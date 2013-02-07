/*
 * Created on 05/apr/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
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
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.Module;
import org.pdfsam.gui.BaseTaskExecutionModule;
import org.pdfsam.gui.about.AboutDialog;
import org.pdfsam.gui.event.OnTaskExecutionModulesLoadedEvent;
import org.pdfsam.gui.preference.PreferencesDialog;
import org.pdfsam.gui.support.SwingUtils;
import org.pdfsam.gui.workspace.LoadWorkspaceAction;
import org.pdfsam.gui.workspace.SaveWorkspaceAction;
import org.pdfsam.module.ModuleCategory;

import bibliothek.gui.dock.common.CControl;
import static org.pdfsam.support.RequireUtils.require;

/**
 * Menu bar for the main frame
 * 
 * @author Andrea Vacondio
 * 
 */
public class MainMenuBar extends JMenuBar {

    private Map<MenuType, JMenu> menus = new HashMap<MenuType, JMenu>();
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
        menuEdit.add(new ShowDialogAction(DefaultI18nContext.getInstance().i18n("Preferences"), PreferencesDialog
                .getInstance()));
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
        menuHelp.add(new ShowDialogAction(DefaultI18nContext.getInstance().i18n("About"), AboutDialog.getInstance()));
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
        require(menu != null, "Unable to fine the given menu: " + type);
        menu.add(new SystemContentAction(control, module));
    }

    @EventSubscriber
    public void initModulesMenu(OnTaskExecutionModulesLoadedEvent event) {
        Map<ModuleCategory, JMenu> moduleSubmenus = new HashMap<ModuleCategory, JMenu>();
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
            require(dialog != null, "Input dialog cannot be null");
            this.dialog = dialog;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!dialog.isVisible()) {
                SwingUtils.centrePositionOnScreen(dialog);
                dialog.setVisible(true);
            }
        }
    }

}
