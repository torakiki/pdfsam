/*
 * This file is part of the PDF Split And Merge source code
 * Created on 17/apr/2014
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

import org.tinylog.Level;

/**
 * Recognized log levels.
 *
 * @author Andrea Vacondio
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

    public static LogLevel toLogLevel(Level intLevel) {
        return switch (intLevel) {
            case ERROR -> ERROR;
            case WARN -> WARN;
            default -> INFO;
        };
    }

}
