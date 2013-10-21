/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 17/ott/2013
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
package org.pdfsam.gui.log;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.support.RequireUtils.require;

/**
 * Event informing that a new message has been logged
 * 
 * @author Andrea Vacondio
 * 
 */
public final class LogMessageEvent {

    private String message;
    private LogMessageLevel level;
    private StackTraceElement[] stack;

    private LogMessageEvent(String message, LogMessageLevel level, StackTraceElement[] stack) {
        require(isNotBlank(message), "Message cannot be empty");
        this.message = message;
        this.level = level;
        this.stack = stack;
    }

    public String getMessage() {
        return message;
    }

    public LogMessageLevel getLevel() {
        return level;
    }

    public StackTraceElement[] getStack() {
        return stack;
    }

    /**
     * @param message
     * @return a standard log message
     */
    public static LogMessageEvent newStandardMessage(String message) {
        return new LogMessageEvent(message, LogMessageLevel.STANDARD, null);
    }

    /**
     * @param message
     * @return a warning log message
     */
    public static LogMessageEvent newWarningMessage(String message) {
        return new LogMessageEvent(message, LogMessageLevel.WARNING, null);
    }

    /**
     * @param message
     * @param stack
     *            stacktrace that might be attached to the log message. It can be null or empty.
     * @return a error message
     */
    public static LogMessageEvent newErrorMessage(String message, StackTraceElement[] stack) {
        return new LogMessageEvent(message, LogMessageLevel.ERROR, stack);
    }

    /**
     * @param message
     * @return an error log message
     */
    public static LogMessageEvent newErrorMessage(String message) {
        return new LogMessageEvent(message, LogMessageLevel.ERROR, null);
    }
}
