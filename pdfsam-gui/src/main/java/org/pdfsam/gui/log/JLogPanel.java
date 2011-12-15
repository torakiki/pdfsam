/*
 * Created on 15/dic/2011
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
package org.pdfsam.gui.log;

import java.awt.GridLayout;

import javax.swing.JPanel;
import javax.swing.JScrollPane;

/**
 * Log panel.
 * 
 * @author Andrea Vacondio
 * 
 */
public class JLogPanel extends JPanel {

    public JLogPanel() {
        setLayout(new GridLayout(1, 1));
        TextPaneAppender.LOG_PANEL.setComponentPopupMenu(new JLogPopupMenu());
        add(new JScrollPane(TextPaneAppender.LOG_PANEL));
    }
}
