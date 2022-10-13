package org.pdfsam.gui.components.log;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/10/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import org.tinylog.Level;
import org.tinylog.core.LogEntry;
import org.tinylog.core.LogEntryValue;
import org.tinylog.writers.AbstractFormatPatternWriter;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Scanner;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Writer that broadcast the entry to the LogStage station
 *
 * @author Andrea Vacondio
 */
public class BroadcastingWriter extends AbstractFormatPatternWriter {

    public BroadcastingWriter() {
        this(Collections.emptyMap());
    }

    /**
     * @param properties Configuration for writer
     */
    public BroadcastingWriter(Map<String, String> properties) {
        super(properties);
    }

    @Override
    public Collection<LogEntryValue> getRequiredLogEntryValues() {
        Collection<LogEntryValue> logEntryValues = super.getRequiredLogEntryValues();
        logEntryValues.add(LogEntryValue.LEVEL);
        return logEntryValues;
    }

    @Override
    public void write(final LogEntry logEntry) {
        try (Scanner scanner = new Scanner(render(logEntry))) {
            while (scanner.hasNextLine()) {
                eventStudio().broadcast(new LogMessage(scanner.nextLine(), LogLevel.toLogLevel(logEntry.getLevel())),
                        "LogStage");
            }
        }
        if (logEntry.getLevel().ordinal() == Level.ERROR.ordinal()) {
            eventStudio().broadcast(new ErrorLoggedEvent());
        }
    }

    @Override
    public void flush() {
    }

    @Override
    public void close() {
    }
}