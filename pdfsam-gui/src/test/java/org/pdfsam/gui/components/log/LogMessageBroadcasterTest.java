/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ago/2014
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
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.classic.spi.ILoggingEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.test.JavaFxThreadInitializeExtension;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadInitializeExtension.class })
@Disabled("Tinylog or Logback, pick one")
//TODO
public class LogMessageBroadcasterTest {

    @RegisterExtension
    public static ClearEventStudioExtension extension = new ClearEventStudioExtension("LogStage");

    private Injector injector;

    @BeforeEach
    public void setUp() {
        injector = Injector.start(new Config());
    }

    static class Config {
        @Provides
        public LogMessageBroadcaster victim(PatternLayoutEncoder encoder) {
            return new LogMessageBroadcaster(encoder);
        }

        @Provides
        public PatternLayoutEncoder encoder() {
            PatternLayoutEncoder encoder = new PatternLayoutEncoder();
            encoder.setPattern("%msg");
            return encoder;
        }
    }

    @Test
    public void infoLog() {
        HitTestListener<LogMessage> listener = new HitTestListener<>();
        eventStudio().add(LogMessage.class, listener, "LogStage");
        var victim = injector.instance(LogMessageBroadcaster.class);
        injector.instance(PatternLayoutEncoder.class);
        ILoggingEvent event = mock(ILoggingEvent.class);
        when(event.getLevel()).thenReturn(Level.INFO);
        when(event.getFormattedMessage()).thenReturn("myMessage");
        victim.start();
        victim.append(event);
        assertTrue(listener.isHit());
    }

    @Test
    public void eventOnError() {
        var victim = injector.instance(LogMessageBroadcaster.class);
        ILoggingEvent event = mock(ILoggingEvent.class);
        when(event.getLevel()).thenReturn(Level.ERROR);
        when(event.getFormattedMessage()).thenReturn("myMessageError");
        HitTestListener<ErrorLoggedEvent> listener = new HitTestListener<>();
        eventStudio().add(ErrorLoggedEvent.class, listener);
        victim.start();
        victim.append(event);
        assertTrue(listener.isHit());
    }
}
