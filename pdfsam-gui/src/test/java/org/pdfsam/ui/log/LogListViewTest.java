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

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.context.UserContext;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;

/**
 * @author Andrea Vacondio
 *
 */
public class LogListViewTest {

    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    private UserContext userContext;

    @Before
    public void setUp() {
        this.userContext = mock(UserContext.class);
    }

    @Test
    public void append() {
        when(userContext.getNumberOfLogRows()).thenReturn(200);
        LogListView victim = new LogListView(userContext);
        victim.appendLog(LogLevel.WARN, "testMessage");
        victim.appendLog(LogLevel.INFO, "anotherTestMessage");
        assertEquals(2, victim.getItems().size());
        assertEquals("testMessage", victim.getItems().get(0).getMessage());
        assertEquals("anotherTestMessage", victim.getItems().get(1).getMessage());
    }

    @Test
    public void appendSizeConstraint() {
        when(userContext.getNumberOfLogRows()).thenReturn(2);
        LogListView victim = new LogListView(userContext);
        victim.appendLog(LogLevel.WARN, "testMessage");
        victim.appendLog(LogLevel.INFO, "anotherTestMessage");
        victim.appendLog(LogLevel.INFO, "anotherTestMessage2");
        victim.appendLog(LogLevel.INFO, "anotherTestMessage3");
        assertEquals(2, victim.getItems().size());
        assertEquals("anotherTestMessage2", victim.getItems().get(0).getMessage());
        assertEquals("anotherTestMessage3", victim.getItems().get(1).getMessage());
    }
}
