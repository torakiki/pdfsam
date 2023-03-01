/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ago/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.context.IntegerPersistentProperty;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadInitializeExtension;

import static java.time.Duration.ofSeconds;
import static org.awaitility.Awaitility.await;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 *
 */
@ExtendWith({ ClearEventStudioExtension.class, JavaFxThreadInitializeExtension.class})
public class LogListViewTest {


    @Test
    public void append()   {
        app().persistentSettings().set(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER, 200);
        LogListView victim = new LogListView();
        victim.onEvent(new LogMessage("testMessage", LogLevel.WARN));
        victim.onEvent(new LogMessage("anotherTestMessage", LogLevel.INFO));
        await().atMost(ofSeconds(2)).until(() -> victim.getItems().size() == 2);
        await().atMost(ofSeconds(2)).until(() -> victim.getItems().get(0).message(), "testMessage"::equals);
        await().atMost(ofSeconds(2)).until(() -> victim.getItems().get(1).message(), "anotherTestMessage"::equals);
    }

    @Test
    public void appendSizeConstraint() {
        app().persistentSettings().set(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER, 2);
        LogListView victim = new LogListView();
        victim.onEvent(new LogMessage("testMessage", LogLevel.WARN));
        victim.onEvent(new LogMessage("anotherTestMessage", LogLevel.INFO));
        victim.onEvent(new LogMessage("anotherTestMessage2", LogLevel.INFO));
        victim.onEvent(new LogMessage("anotherTestMessage3", LogLevel.INFO));
        await().atMost(ofSeconds(2)).until(() -> victim.getItems().size() == 2);
        await().atMost(ofSeconds(2)).until(() -> victim.getItems().get(0).message(), "anotherTestMessage2"::equals);
        await().atMost(ofSeconds(2)).until(() -> victim.getItems().get(1).message(), "anotherTestMessage3"::equals);
    }

    @Test
    public void maxNumberOfLogRowsChanged() {
        app().persistentSettings().set(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER, 5);
        LogListView victim = new LogListView();
        victim.onEvent(new LogMessage("testMessage", LogLevel.WARN));
        victim.onEvent(new LogMessage("anotherTestMessage", LogLevel.INFO));
        victim.onEvent(new LogMessage("anotherTestMessage2", LogLevel.INFO));
        victim.onEvent(new LogMessage("anotherTestMessage3", LogLevel.INFO));
        victim.onEvent(new LogMessage("anotherTestMessage4", LogLevel.INFO));
        await().atMost(ofSeconds(2)).until(() -> victim.getItems().size() == 5);
        app().persistentSettings().set(IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER, 2);
        eventStudio().broadcast(new MaxLogRowsChangedEvent());
        await().atMost(ofSeconds(2)).until(() -> victim.getItems().size() == 2);
        await().atMost(ofSeconds(2)).until(() -> victim.getItems().get(0).message(), "anotherTestMessage3"::equals);
        await().atMost(ofSeconds(2)).until(() -> victim.getItems().get(1).message(), "anotherTestMessage4"::equals);
    }
}
