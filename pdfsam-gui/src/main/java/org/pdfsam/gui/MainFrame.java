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
package org.pdfsam.gui;

import java.awt.BorderLayout;

import javax.swing.ImageIcon;
import javax.swing.JFrame;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.log.JLogPanel;
import org.pdfsam.gui.menu.MainMenuBar;
import org.pdfsam.gui.menu.MenuType;
import org.pdfsam.gui.status.StatusPanel;
import org.pdfsam.module.BaseTaskExecutionModule;
import org.pdfsam.module.Module;
import org.pdfsam.module.TaskExecutionModulesLoadedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import bibliothek.gui.dock.common.CControl;
import bibliothek.gui.dock.common.CMinimizeArea;
import bibliothek.gui.dock.common.CWorkingArea;
import bibliothek.gui.dock.common.DefaultSingleCDockable;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Application main frame.
 * 
 * @author Andrea Vacondio
 * 
 */
public class MainFrame extends JFrame {

    private static final Logger LOG = LoggerFactory.getLogger(MainFrame.class);

    private CWorkingArea workingArea;
    private MainMenuBar menuBar;

    public MainFrame(String title) {
        super(title);
        init();
        AnnotationProcessor.process(this);
    }

    private void init() {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(640, 480);
    }

    private void initLogWindow(PdfsamContentArea contentArea) {
        CMinimizeArea minimizedArea = contentArea.getMinimizeArea();
        minimizedArea.add(new StatusPanel(), BorderLayout.LINE_END);
        DefaultSingleCDockable logDockable = new DefaultSingleCDockable("Log", new ImageIcon(
                MainFrame.class.getResource("/images/log.png")), "Log Viewer", new JLogPanel());
        logDockable.setLocation(minimizedArea.getStationLocation());
        contentArea.getControl().addDockable(logDockable);
        logDockable.setVisible(true);
        LOG.debug(DefaultI18nContext.getInstance().i18n("Log Viewer panel initialized"));
    }

    public void addSystemContentAction(MenuType type, Module module) {
        DefaultSingleCDockable dockable = new DefaultSingleCDockable(module.getDescriptor().getId(), module
                .getDescriptor().getIcon(), module.getDescriptor().getName(), module.getModulePanel());
        dockable.setCloseable(true);
        workingArea.add(dockable);
        dockable.setMaximizable(false);
        menuBar.addSystemContentAction(type, module);
    }

    @EventSubscriber
    public void initModules(TaskExecutionModulesLoadedEvent event) {
        for (BaseTaskExecutionModule currentModule : event.getModules()) {
            DefaultSingleCDockable dockable = new DefaultSingleCDockable(currentModule.getDescriptor().getId(),
                    currentModule.getDescriptor().getIcon(), currentModule.getDescriptor().getName(),
                    currentModule.getModulePanel());
            dockable.setCloseable(true);
            dockable.setMaximizable(false);
            workingArea.add(dockable);
        }
    }

    public void initControl(CControl control) {
        requireNotNull(control, "Control cannot be null");
        PdfsamContentArea contentArea = new PdfsamContentArea(control, "PdfsamContentArea");
        control.addStationContainer(contentArea);
        add(contentArea);

        workingArea = contentArea.getWorkingArea();
        menuBar = new MainMenuBar(control);
        getRootPane().setJMenuBar(menuBar);
        initLogWindow(contentArea);
        contentArea.setVisible(true);
    }

}
