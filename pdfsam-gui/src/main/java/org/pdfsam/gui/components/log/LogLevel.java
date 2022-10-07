/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 17/apr/2014
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
package org.pdfsam.gui.components.log;

import ch.qos.logback.classic.Level;

/**
 * Recognized log levels.
 * 
 * @author Andrea Vacondio
 *
 */
enum LogLevel {
    INFO {
        @Override
        public String style() {
            return "info-log";
        }
    },
    WARN {
        @Override
        public String style() {
            return "warn-log";
        }
    },
    ERROR {
        @Override
        public String style() {
            return "error-log";
        }
    };

    public abstract String style();

    public static LogLevel toLogLevel(int intLevel) {
        return switch (intLevel) {
            case Level.ERROR_INT -> ERROR;
            case Level.WARN_INT -> WARN;
            default -> INFO;
        };
    }

}
