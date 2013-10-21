/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.io.IOException;
import java.io.StringWriter;

import org.apache.commons.io.output.WriterOutputStream;
import org.apache.commons.lang3.StringUtils;
import org.bushe.swing.event.EventBus;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

/**
 * A Logback appender that dispatch the log event as a {@link LogMessageEvent} after the message has been formatted.
 * 
 * @author Andrea Vacondio
 * 
 */
public class DispatchAppender extends AppenderBase<ILoggingEvent> {
    private PatternLayoutEncoder encoder;

    @Override
    public void start() {
        if (this.encoder == null) {
            addError("No layout set for the appender named [" + name + "].");
            return;
        }
        super.start();
    }

    @Override
    public void append(ILoggingEvent event) {
        StringWriter writer = new StringWriter();
        try {
            encoder.init(new WriterOutputStream(writer));
            encoder.doEncode(event);
        } catch (IOException e) {
            e.printStackTrace();
        }
        doAppendMessage(writer.toString(), event);
    }

    private void doAppendMessage(String message, ILoggingEvent event) {
        if (StringUtils.isNotBlank(message)) {
            EventBus.publish(createLogMessageEvent(message, event));
        }
    }

    private LogMessageEvent createLogMessageEvent(String message, ILoggingEvent event) {
        if (event.getLevel().isGreaterOrEqual(Level.ERROR)) {
            if (event.hasCallerData()) {
                return LogMessageEvent.newErrorMessage(message, event.getCallerData());
            }
            return LogMessageEvent.newErrorMessage(message);
        }
        if (event.getLevel().toInt() == Level.WARN_INT) {
            return LogMessageEvent.newWarningMessage(message);
        }
        return LogMessageEvent.newStandardMessage(message);
    }

    public PatternLayoutEncoder getEncoder() {
        return encoder;
    }

    public void setEncoder(PatternLayoutEncoder encoder) {
        this.encoder = encoder;
    }

}
