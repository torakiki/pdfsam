/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ago/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.io.IOException;

import javax.inject.Inject;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeJavaFxThreadRule;
import org.sejda.eventstudio.Listener;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.classic.spi.ILoggingEvent;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class LogMessageBroadcasterTest {


    @Inject
    private ApplicationContext applicationContext;
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule("LogStage");
    @Rule
    public InitializeJavaFxThreadRule threadFx = new InitializeJavaFxThreadRule();

    @Configuration
    @Lazy
    static class Config {
        @Bean
        public LogMessageBroadcaster victim() {
            return new LogMessageBroadcaster(encoder());
        }

        @Bean
        public PatternLayoutEncoder encoder() {
            PatternLayoutEncoder encoder = new PatternLayoutEncoder();
            encoder.setPattern("%msg");
            return spy(encoder);
        }
    }

    @Test
    public void infoLog() throws IOException {
        Listener<LogMessage> listener = mock(Listener.class);
        eventStudio().add(LogMessage.class, listener, "LogStage");
        LogMessageBroadcaster victim = applicationContext.getBean(LogMessageBroadcaster.class);
        PatternLayoutEncoder encoder = applicationContext.getBean(PatternLayoutEncoder.class);
        ILoggingEvent event = mock(ILoggingEvent.class);
        when(event.getLevel()).thenReturn(Level.INFO);
        when(event.getFormattedMessage()).thenReturn("myMessage");
        victim.start();
        victim.append(event);
        verify(encoder).doEncode(event);
        verify(listener).onEvent(any(LogMessage.class));
    }

    @Test
    public void eventOnError() {
        LogMessageBroadcaster victim = applicationContext.getBean(LogMessageBroadcaster.class);
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
