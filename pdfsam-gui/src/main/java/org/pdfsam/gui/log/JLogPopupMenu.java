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

import javax.swing.JPopupMenu;

/**
 * Popup component for the log panel.
 * 
 * @author Andrea Vacondio
 * 
 */
class JLogPopupMenu extends JPopupMenu {

    public JLogPopupMenu() {
        initComponent();
    }

    private void initComponent() {
        this.add(new CopyAction());
        this.add(new ClearLogAction());
        this.add(new SelectAllAction());
        this.addSeparator();
        this.add(new SaveLogAction());
    }
}
