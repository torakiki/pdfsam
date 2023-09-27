/*
 * This file is part of the PDF Split And Merge source code
 * Created on 11/apr/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.components.notification;

import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * Request for a notification to be removed/hidden
 *
 * @author Andrea Vacondio
 */
record RemoveNotificationRequest(String id) {

    public RemoveNotificationRequest {
        requireNotBlank(id, "Cannot request to remove a notification without supplying the id");
    }
}
