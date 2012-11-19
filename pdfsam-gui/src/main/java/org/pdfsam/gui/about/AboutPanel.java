/*
 * Created on 03/apr/2012
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
package org.pdfsam.gui.about;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.pdfsam.Pdfsam;
import org.pdfsam.context.DefaultI18nContext;
import org.swingplus.JHyperlink;

/**
 * Panel displaying About information
 * 
 * @author Andrea Vacondio
 * 
 */
class AboutPanel extends JPanel {

    public AboutPanel() {
        init();
    }

    private void init() {
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBackground(Color.WHITE);
        add(Box.createVerticalGlue());
        JLabel appName = new JLabel(String.format("PDF Split and Merge %s", Pdfsam.PACKAGE));
        Font f = appName.getFont();
        appName.setFont(f.deriveFont(f.getStyle() | Font.BOLD));
        add(appName);
        add(Box.createRigidArea(new Dimension(0, 10)));
        add(new JLabel(String.format("ver. %s", Pdfsam.VERSION)));
        Dimension labelSpace = new Dimension(0, 5);
        add(Box.createRigidArea(labelSpace));
        add(new JLabel("Copyright 2012 by Andrea Vacondio"));
        add(Box.createRigidArea(labelSpace));
        add(new JLabel(System.getProperty("java.runtime.name") + " " + System.getProperty("java.runtime.version")));
        add(Box.createRigidArea(labelSpace));
        add(new JLabel(DefaultI18nContext.getInstance().i18n("Max memory {0}Mb",
                Long.toString(Runtime.getRuntime().maxMemory() / 1048576))));
        add(Box.createRigidArea(labelSpace));
        add(new JHyperlink("www.pdfsam.org", "http://www.pdfsam.org"));
        add(Box.createRigidArea(labelSpace));
        add(Box.createVerticalGlue());
    }
}
