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
package org.pdfsam.gui;

import java.awt.GridLayout;

import javax.swing.ImageIcon;
import javax.swing.JFrame;

import org.noos.xing.mydoggy.DockedTypeDescriptor;
import org.noos.xing.mydoggy.RepresentativeAnchorDescriptor;
import org.noos.xing.mydoggy.ToolWindow;
import org.noos.xing.mydoggy.ToolWindowAnchor;
import org.noos.xing.mydoggy.ToolWindowType;
import org.noos.xing.mydoggy.plaf.MyDoggyToolWindowManager;
import org.pdfsam.Pdfsam;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.log.JLogPanel;
import org.pdfsam.gui.menu.MainMenuBar;
import org.pdfsam.gui.menu.MenuType;
import org.pdfsam.gui.status.StatusDockableDescriptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Application main frame.
 * 
 * @author Andrea Vacondio
 * 
 */
public class MainFrame extends JFrame {

    private static final Logger LOG = LoggerFactory.getLogger(MainFrame.class);

    // TODO locale for tool window manager
    private MyDoggyToolWindowManager toolWindowManager = new MyDoggyToolWindowManager();
    private MainMenuBar menuBar;

    public MainFrame() {
        super(String.format("PDF Split and Merge %s ver. %s", Pdfsam.PACKAGE, Pdfsam.VERSION));
        init();
        initLogWindow();
        initMenu();
    }

    private void initMenu() {
        menuBar = new MainMenuBar(toolWindowManager.getContentManager());
        getRootPane().setJMenuBar(menuBar);
    }

    private void init() {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(new GridLayout(1, 1));
        setIconImage(new ImageIcon(MainFrame.class.getResource("/images/pdfsam_" + Pdfsam.PACKAGE + ".png")).getImage());
        setSize(640, 480);
        toolWindowManager.getToolWindowManagerDescriptor().setNumberingEnabled(false);
        getContentPane().add(toolWindowManager);
    }

    private void initLogWindow() {
        toolWindowManager
                .registerToolWindow("Log", "Log", new ImageIcon(MainFrame.class.getResource("/images/log.png")),
                        new JLogPanel(), ToolWindowAnchor.BOTTOM);
        ToolWindow logToolWindow = toolWindowManager.getToolWindow("Log");
        logToolWindow.setLockedOnAnchor(true);

        // RepresentativeAnchorDescriptor
        RepresentativeAnchorDescriptor<ToolWindow> representativeAnchorDescriptor = logToolWindow
                .getRepresentativeAnchorDescriptor();
        representativeAnchorDescriptor.setTitle("Console");
        representativeAnchorDescriptor.setPreviewEnabled(false);

        // DockedTypeDescriptor
        DockedTypeDescriptor dockedTypeDescriptor = (DockedTypeDescriptor) logToolWindow
                .getTypeDescriptor(ToolWindowType.DOCKED);
        dockedTypeDescriptor.setIdVisibleOnTitleBar(false);
        dockedTypeDescriptor.setHideRepresentativeButtonOnVisible(false);
        dockedTypeDescriptor.setDockLength(300);
        dockedTypeDescriptor.setPopupMenuEnabled(false);

        logToolWindow.setAvailable(true);
        logToolWindow.setVisible(false);

        initStatusPanel();

        LOG.debug(DefaultI18nContext.getInstance().i18n("Console panel initialized"));
    }

    private void initStatusPanel() {
        StatusDockableDescriptor statusDescriptor = new StatusDockableDescriptor(toolWindowManager);
        statusDescriptor.setAvailable(true);
        statusDescriptor.setAnchor(ToolWindowAnchor.BOTTOM, 0);
    }

    public void addSystemContentAction(MenuType type, Module module) {
        menuBar.addSystemContentAction(type, module);
    }
}
