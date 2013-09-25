/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 24/set/2013
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
package org.pdfsam.gui.event;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Template to execute an event subscriber callback when allowed by the {@link EventNamespace}
 * 
 * @author Andrea Vacondio
 * 
 */
public final class EventSubscriberTemplate implements OnGoingTemplateCreation, EventSubscriberCallbackExecutor {

    private EventNamespace namespace;
    private BaseEvent event;

    private EventSubscriberTemplate(BaseEvent event) {
        requireNotNull(event, "Event cannot be null");
        this.event = event;
    }

    public static OnGoingTemplateCreation ifEvent(BaseEvent event) {
        return new EventSubscriberTemplate(event);
    }

    public EventSubscriberCallbackExecutor routesTo(EventNamespace namespace) {
        requireNotNull(namespace, "Namespace cannot be null");
        this.namespace = namespace;
        return this;
    }

    public void execute(EventSubscriberCallback callback) {
        if (event.getNamespace().isParentOf(namespace)) {
            callback.exec(event);
        }
    }

}
