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

import javax.swing.AbstractAction;

import org.pdfsam.gui.module.Module;

import bibliothek.gui.dock.common.CControl;
import bibliothek.gui.dock.common.SingleCDockable;

/**
 * Action used to show or hide a system content panel.
 * 
 * @author Andrea Vacondio
 * 
 */
class SystemContentAction extends AbstractAction {

    private CControl control;
    private Module module;

    SystemContentAction(CControl control, Module module) {
        super(module.getDescriptor().getName());
        this.control = control;
        this.module = module;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        SingleCDockable content = control.getSingleDockable(module.getDescriptor().getId());
        if (content != null && !content.isShowing()) {
            content.setVisible(true);
        }
    }

}
