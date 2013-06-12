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

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import static org.pdfsam.support.RequireUtils.require;

/**
 * Namespace for the events. The same component type can be subscribed to the same event on different namespaces.
 * 
 * @author Andrea Vacondio
 * @see EventNamespace#NULL
 * 
 */
public final class EventNamespace {

    public static final EventNamespace NULL = new EventNamespace("");

    private String namespaceId;

    private EventNamespace(String namespaceId) {
        this.namespaceId = namespaceId;
    }

    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * @param target
     * @return true if this is the {@link EventNamespace#NULL} or is a parent of target
     */
    public boolean isParentOf(EventNamespace target) {
        return !equals(target) && target.getNamespaceId().startsWith(namespaceId);
    }

    @Override
    public String toString() {
        return namespaceId;
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(namespaceId).toHashCode();
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof EventNamespace)) {
            return false;
        }
        EventNamespace otherClass = (EventNamespace) other;
        return new EqualsBuilder().append(namespaceId, otherClass.namespaceId).isEquals();
    }

    /**
     * 
     * @param namespaceId
     * @return a new parent namespace with the given id
     */
    public static EventNamespace newParentInstance(String namespaceId) {
        require(isNotBlank(namespaceId), "Namespace identifier cannot be blank");
        return new EventNamespace(namespaceId);
    }

    /**
     * 
     * @param parent
     * @param namespaceId
     * @return a new child namespace with the given id
     */
    public static EventNamespace newChildInstance(EventNamespace parent, String namespaceId) {
        require(isNotBlank(namespaceId), "Namespace identifier cannot be blank");
        return new EventNamespace(String.format("%s.%s", parent.namespaceId, namespaceId));
    }
}
