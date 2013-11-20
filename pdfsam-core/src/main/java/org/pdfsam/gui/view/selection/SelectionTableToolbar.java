/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/giu/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.gui.view.selection;

import javax.swing.JToolBar;

import org.pdfsam.gui.event.String;
import org.pdfsam.gui.event.ModuleOwned;

import static org.pdfsam.gui.view.selection.SelectionTableToolbarButtons.addButton;
import static org.pdfsam.gui.view.selection.SelectionTableToolbarButtons.clearButton;
import static org.pdfsam.gui.view.selection.SelectionTableToolbarButtons.moveDownButton;
import static org.pdfsam.gui.view.selection.SelectionTableToolbarButtons.moveUpButton;
import static org.pdfsam.gui.view.selection.SelectionTableToolbarButtons.removeButton;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Toolbar for the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTableToolbar extends JToolBar implements ModuleOwned {

    private String namespace = String.NULL;

    public SelectionTableToolbar(String namespace) {
        requireNotNull(namespace, "Event namespace cannot be null");
        this.namespace = namespace;
        setFloatable(false);
        setRollover(true);
        add(addButton(namespace));
        addSeparator();
        add(clearButton(namespace));
        add(removeButton(namespace));
        addSeparator();
        add(moveUpButton(namespace));
        add(moveDownButton(namespace));
        setBorderPainted(false);
    }

    public String getOwnerModule() {
        return namespace;
    }

}
