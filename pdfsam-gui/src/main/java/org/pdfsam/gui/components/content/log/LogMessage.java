/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/apr/2014
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
package org.pdfsam.gui.components.content.log;

import org.apache.commons.lang3.ObjectUtils;

/**
 * Model for a Log message
 *
 * @author Andrea Vacondio
 */
record LogMessage(String message, LogLevel level) {

    LogMessage(String message, LogLevel level) {
        this.message = message;
        this.level = ObjectUtils.getIfNull(level, LogLevel.INFO);
    }

    @Override
    public String toString() {
        return message();
    }

}
