/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/apr/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.log;

import static org.pdfsam.support.RequireUtils.requireNotBlank;

import org.apache.commons.lang3.ObjectUtils;

/**
 * Model for a Log message
 * 
 * @author Andrea Vacondio
 *
 */
class LogMessage {

    private String message;
    private LogLevel level;

    public LogMessage(String message, LogLevel level) {
        requireNotBlank(message, "Cannot create an empty log message");
        this.message = message;
        this.level = ObjectUtils.defaultIfNull(level, LogLevel.INFO);
    }

    public String getMessage() {
        return message;
    }

    public LogLevel getLevel() {
        return level;
    }

    @Override
    public String toString() {
        return getMessage();
    }

}
