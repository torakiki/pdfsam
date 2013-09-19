/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/giu/2013
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
package org.pdfsam.gui.view;

import javax.swing.Action;
import javax.swing.JButton;

import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.WithEventNamespace;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * {@link JButton} {@link WithEventNamespace}
 * 
 * @author Andrea Vacondio
 * 
 */
public class JButtonWithNamespace extends JButton implements WithEventNamespace {

    private EventNamespace namespace = EventNamespace.NULL;

    public JButtonWithNamespace(Action a, EventNamespace namespace) {
        super(a);
        requireNotNull(namespace, "Namespace cannot be null");
        this.namespace = namespace;
    }

    public EventNamespace getEventNamespace() {
        return namespace;
    }
}
