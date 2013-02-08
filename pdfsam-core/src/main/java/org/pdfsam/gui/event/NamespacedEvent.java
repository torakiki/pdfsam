/*
 * Created on 08/feb/2013
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
package org.pdfsam.gui.event;

import org.pdfsam.support.RequireUtils;

/**
 * Event having a namespace.
 * 
 * @author Andrea Vacondio
 * 
 */
public class NamespacedEvent {

    private EventNamespace namespace = EventNamespace.NULL;

    public NamespacedEvent(EventNamespace namespace) {
        RequireUtils.require(namespace != null, "Namespace cannot be null");
        this.namespace = namespace;
    }

    public EventNamespace getNamespace() {
        return namespace;
    }

}
