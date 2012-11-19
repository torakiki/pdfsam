/*
 * Created on 08/apr/2012
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
package org.pdfsam.gui.workspace;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.KeyStroke;

import org.bushe.swing.event.EventBus;
import org.pdfsam.context.DefaultI18nContext;

/**
 * Load the workspace action.
 * 
 * @author Andrea Vacondio
 * 
 */
public class LoadWorkspaceAction extends AbstractAction {

    public LoadWorkspaceAction() {
        super(DefaultI18nContext.getInstance().i18n("Load"));
        this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_L, InputEvent.CTRL_DOWN_MASK));
        this.setEnabled(true);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        EventBus.publish(new LoadWorkspaceEvent());
    }

}
