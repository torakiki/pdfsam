/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ago/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeJavaFxThreadRule;
import org.sejda.eventstudio.Listener;
import org.sejda.injector.Injector;
import org.sejda.injector.Provides;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.classic.spi.ILoggingEvent;

/**
 * @author Andrea Vacondio
 *
 */
public class LogMessageBroadcasterTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule("LogStage");
    @Rule
    public InitializeJavaFxThreadRule threadFx = new InitializeJavaFxThreadRule();
    private Injector injector;

    @Before
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
            return spy(encoder);
        }
    }

    @Test
    public void infoLog() {
        Listener<LogMessage> listener = mock(Listener.class);
        eventStudio().add(LogMessage.class, listener, "LogStage");
        LogMessageBroadcaster victim = injector.instance(LogMessageBroadcaster.class);
        PatternLayoutEncoder encoder = injector.instance(PatternLayoutEncoder.class);
        ILoggingEvent event = mock(ILoggingEvent.class);
        when(event.getLevel()).thenReturn(Level.INFO);
        when(event.getFormattedMessage()).thenReturn("myMessage");
        victim.start();
        victim.append(event);
        verify(listener).onEvent(any(LogMessage.class));
    }

    @Test
    public void eventOnError() {
        LogMessageBroadcaster victim = injector.instance(LogMessageBroadcaster.class);
        ILoggingEvent event = mock(ILoggingEvent.class);
        when(event.getLevel()).thenReturn(Level.ERROR);
        when(event.getFormattedMessage()).thenReturn("myMessage");
        Listener<ErrorLoggedEvent> listener = mock(Listener.class);
        eventStudio().add(ErrorLoggedEvent.class, listener);
        victim.start();
        victim.append(event);
        verify(listener, timeout(1000).times(1)).onEvent(any());
    }
}
