package org.pdfsam.gui.components.sidebar;

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.gui.components.content.log.ErrorLoggedEvent;
import org.pdfsam.gui.components.content.log.LogContentItem;
import org.pdfsam.gui.components.content.log.LogPane;
import org.pdfsam.model.ui.ShowLogMessagesRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxAssert;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.matcher.base.NodeMatchers;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@pdfsam.org).
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
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
class LogButtonTest {
    private FxRobot robot;
    private LogButton victim;

    @Start
    public void start(Stage stage) {
        this.victim = new LogButton(new LogContentItem(mock(LogPane.class)));
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void onClick() {
        Listener<ShowLogMessagesRequest> listener = mock(Listener.class);
        eventStudio().add(ShowLogMessagesRequest.class, listener);
        robot.clickOn(victim);
        verify(listener).onEvent(any(ShowLogMessagesRequest.class));
    }

    @Test
    public void onErrorLoggedEvent() {
        eventStudio().broadcast(new ErrorLoggedEvent());
        FxAssert.verifyThat(".notification", NodeMatchers.isVisible());
    }

    @Test
    @DisplayName("No notification when button is selected")
    public void onErrorLoggedEventSelected() {
        victim.setSelected(true);
        var event = new ErrorLoggedEvent();
        eventStudio().broadcast(event);
        FxAssert.verifyThat(".notification", NodeMatchers.isInvisible());
    }

    @Test
    @DisplayName("Hide the notification when others opened the log panel")
    public void onShowErrorMessagesRequest() {
        eventStudio().broadcast(new ErrorLoggedEvent());
        FxAssert.verifyThat(".notification", NodeMatchers.isVisible());
        eventStudio().broadcast(new ShowLogMessagesRequest());
        FxAssert.verifyThat(".notification", NodeMatchers.isInvisible());
    }

}