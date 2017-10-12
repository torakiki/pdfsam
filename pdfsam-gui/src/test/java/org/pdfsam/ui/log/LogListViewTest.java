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

import static com.google.code.tempusfugit.temporal.Duration.seconds;
import static com.google.code.tempusfugit.temporal.Timeout.timeout;
import static com.google.code.tempusfugit.temporal.WaitFor.waitOrTimeout;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.concurrent.TimeoutException;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.context.UserContext;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeJavaFxThreadRule;

/**
 * @author Andrea Vacondio
 *
 */
public class LogListViewTest {

    @Rule
    public InitializeJavaFxThreadRule javaFxThread = new InitializeJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private UserContext userContext;

    @Before
    public void setUp() {
        this.userContext = mock(UserContext.class);
    }

    @Test
    public void append() throws InterruptedException, TimeoutException {
        when(userContext.getNumberOfLogRows()).thenReturn(200);
        LogListView victim = new LogListView(userContext);
        victim.onEvent(new LogMessage("testMessage", LogLevel.WARN));
        victim.onEvent(new LogMessage("anotherTestMessage", LogLevel.INFO));
        waitOrTimeout(() -> victim.getItems().size() == 2, timeout(seconds(2)));
        waitOrTimeout(() -> "testMessage".equals(victim.getItems().get(0).getMessage()), timeout(seconds(2)));
        waitOrTimeout(() -> "anotherTestMessage".equals(victim.getItems().get(1).getMessage()), timeout(seconds(2)));
    }

    @Test
    public void appendSizeConstraint() throws InterruptedException, TimeoutException {
        when(userContext.getNumberOfLogRows()).thenReturn(2);
        LogListView victim = new LogListView(userContext);
        victim.onEvent(new LogMessage("testMessage", LogLevel.WARN));
        victim.onEvent(new LogMessage("anotherTestMessage", LogLevel.INFO));
        victim.onEvent(new LogMessage("anotherTestMessage2", LogLevel.INFO));
        victim.onEvent(new LogMessage("anotherTestMessage3", LogLevel.INFO));
        waitOrTimeout(() -> victim.getItems().size() == 2, timeout(seconds(2)));
        waitOrTimeout(() -> "anotherTestMessage2".equals(victim.getItems().get(0).getMessage()), timeout(seconds(2)));
        waitOrTimeout(() -> "anotherTestMessage3".equals(victim.getItems().get(1).getMessage()), timeout(seconds(2)));
    }

    @Test
    public void maxNumberOfLogRowsChanged() throws InterruptedException, TimeoutException {
        when(userContext.getNumberOfLogRows()).thenReturn(5);
        LogListView victim = new LogListView(userContext);
        victim.onEvent(new LogMessage("testMessage", LogLevel.WARN));
        victim.onEvent(new LogMessage("anotherTestMessage", LogLevel.INFO));
        victim.onEvent(new LogMessage("anotherTestMessage2", LogLevel.INFO));
        victim.onEvent(new LogMessage("anotherTestMessage3", LogLevel.INFO));
        victim.onEvent(new LogMessage("anotherTestMessage4", LogLevel.INFO));
        waitOrTimeout(() -> victim.getItems().size() == 5, timeout(seconds(2)));
        when(userContext.getNumberOfLogRows()).thenReturn(2);
        eventStudio().broadcast(new MaxLogRowsChangedEvent());
        waitOrTimeout(() -> victim.getItems().size() == 2, timeout(seconds(2)));
        waitOrTimeout(() -> "anotherTestMessage3".equals(victim.getItems().get(0).getMessage()), timeout(seconds(2)));
        waitOrTimeout(() -> "anotherTestMessage4".equals(victim.getItems().get(1).getMessage()), timeout(seconds(2)));
    }
}
