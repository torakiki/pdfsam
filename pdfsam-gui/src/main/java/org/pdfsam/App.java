/*
 * Created on 14/dic/2011
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
package org.pdfsam;

import java.awt.GridLayout;

import javax.swing.ImageIcon;
import javax.swing.JFrame;

import org.noos.xing.mydoggy.DockedTypeDescriptor;
import org.noos.xing.mydoggy.RepresentativeAnchorDescriptor;
import org.noos.xing.mydoggy.ToolWindow;
import org.noos.xing.mydoggy.ToolWindowAnchor;
import org.noos.xing.mydoggy.ToolWindowType;
import org.noos.xing.mydoggy.plaf.MyDoggyToolWindowManager;
import org.pdfsam.gui.log.JLogPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Andrea Vacondio
 * 
 */
public final class App {
    private static final Logger LOG = LoggerFactory.getLogger(App.class);
    public static final JFrame MAIN_FRAME = new JFrame();

    private App() {
        // hide
    }

    public static void main(String[] args) {
        MAIN_FRAME.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        MAIN_FRAME.setLayout(new GridLayout(1, 1));
        MAIN_FRAME.setTitle(String.format("PDF Split and Merge %s ver. %s", Pdfsam.PACKAGE, Pdfsam.VERSION));
        MAIN_FRAME.setIconImage(new ImageIcon(App.class.getResource("/images/pdfsam_" + Pdfsam.PACKAGE + ".png"))
                .getImage());
        MAIN_FRAME.setSize(640, 480);
        MyDoggyToolWindowManager toolWindowManager = new MyDoggyToolWindowManager();
        toolWindowManager.getToolWindowManagerDescriptor().setNumberingEnabled(false);

        MAIN_FRAME.getContentPane().add(toolWindowManager);
        setUpLogPanel(toolWindowManager);
        MAIN_FRAME.setVisible(true);
    }

    /**
     * @param toolWindowManager
     * @return
     */
    private static ToolWindow setUpLogPanel(MyDoggyToolWindowManager toolWindowManager) {
        toolWindowManager.registerToolWindow("Log", // Id
                "Log", // Title
                new ImageIcon(App.class.getResource("/images/log.png")), // Icon
                new JLogPanel(), // Component
                ToolWindowAnchor.BOTTOM); // Anchor
        ToolWindow logToolWindow = toolWindowManager.getToolWindow("Log");

        // RepresentativeAnchorDescriptor
        RepresentativeAnchorDescriptor representativeAnchorDescriptor = logToolWindow
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
        return logToolWindow;
    }
}
