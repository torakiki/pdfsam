/*
 * Created on 19/giu/2013
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
package org.pdfsam.gui.view.selection;

import javax.swing.JToolBar;

import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.WithEventNamespace;

/**
 * Toolbar for the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTableToolbar extends JToolBar implements WithEventNamespace {

    private EventNamespace namespace = EventNamespace.NULL;

    public SelectionTableToolbar(EventNamespace namespace) {
        this.namespace = namespace;
        setFloatable(false);
        setRollover(true);
        add(new SelectionToolbarButton(new AddAction(namespace), namespace));
        addSeparator();
        add(new SelectionToolbarButton(new ClearAction(namespace), namespace));
        setBorderPainted(false);
    }

    public EventNamespace getEventNamespace() {
        return namespace;
    }

}
