/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
