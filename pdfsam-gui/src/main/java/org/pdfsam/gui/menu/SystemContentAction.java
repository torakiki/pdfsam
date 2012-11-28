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

import javax.swing.AbstractAction;
import javax.swing.JScrollPane;

import org.noos.xing.mydoggy.Content;
import org.noos.xing.mydoggy.ContentManager;
import org.noos.xing.mydoggy.ContentUI;
import org.pdfsam.gui.Module;

/**
 * Action used to show or hide a system content panel.
 * 
 * @author Andrea Vacondio
 * 
 */
class SystemContentAction extends AbstractAction {

    private ContentManager contentManager;
    private Module module;

    SystemContentAction(ContentManager contentManager, Module module) {
        super(module.getDescriptor().getName());
        this.contentManager = contentManager;
        this.module = module;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        Content content = contentManager.getContent(module.getDescriptor().getId());
        if (content == null) {
            content = contentManager.addContent(module.getDescriptor().getId(), module.getDescriptor().getName(),
                    module.getDescriptor().getIcon(), new JScrollPane(module.getModulePanel()));
            ContentUI contentUI = content.getContentUI();
            contentUI.setDetachable(false);
            contentUI.setMinimizable(false);
            contentUI.setCloseable(true);
        }
        content.setSelected(true);
    }

}
