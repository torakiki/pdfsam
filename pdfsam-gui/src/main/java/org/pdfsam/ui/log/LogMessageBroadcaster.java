/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static java.util.Objects.nonNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Scanner;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;
import ch.qos.logback.core.Layout;

/**
 * A Logback appender appending log messages to a {@link LogListView}.
 * 
 * @author Andrea Vacondio
 * 
 */
public class LogMessageBroadcaster extends AppenderBase<ILoggingEvent> {

    private PatternLayoutEncoder encoder;

    public LogMessageBroadcaster(PatternLayoutEncoder encoder) {
        this.encoder = encoder;
        LoggerContext loggerContext = (LoggerContext) LoggerFactory.getILoggerFactory();
        encoder.setContext(loggerContext);
        setContext(loggerContext);
        encoder.start();
        ((LoggerContext) this.getContext()).getLogger(Logger.ROOT_LOGGER_NAME).addAppender(this);
    }

    @Override
    public void append(ILoggingEvent event) {
        if (!isStarted()) {
            return;
        }
        Layout<ILoggingEvent> layout = encoder.getLayout();
        if (nonNull(layout)) {
            doAppendMessage(encoder.getLayout().doLayout(event), event);
        }
    }

    private void doAppendMessage(String message, ILoggingEvent event) {
        if (StringUtils.isNotBlank(message)) {
            try (Scanner scanner = new Scanner(message)) {
                while (scanner.hasNextLine()) {
                    eventStudio().broadcast(
                            new LogMessage(scanner.nextLine(), LogLevel.toLogLevel(event.getLevel().toInt())),
                            "LogStage");
                }
            }
            if (event.getLevel().isGreaterOrEqual(Level.ERROR)) {
                eventStudio().broadcast(new ErrorLoggedEvent());
            }
        }
    }

    public PatternLayoutEncoder getEncoder() {
        return encoder;
    }

    public void setEncoder(PatternLayoutEncoder encoder) {
        this.encoder = encoder;
    }
}
