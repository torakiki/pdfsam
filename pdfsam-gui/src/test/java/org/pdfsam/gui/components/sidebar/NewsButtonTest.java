package org.pdfsam.gui.components.sidebar;

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.news.LatestNewsResponse;
import org.pdfsam.model.news.NewsData;
import org.pdfsam.model.news.ToggleNewsPanelRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxAssert;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.matcher.base.NodeMatchers;

import java.time.LocalDate;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.testfx.util.WaitForAsyncUtils.waitForFxEvents;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
public class NewsButtonTest {

    private FxRobot robot;
    private NewsButton victim;

    @Start
    public void start(Stage stage) {
        this.victim = new NewsButton();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void initialState() {
        assertTrue(victim.getWrapped().isDisabled());
    }

    @Test
    public void onClick() {
        victim.getWrapped().setDisable(false);
        Listener<ToggleNewsPanelRequest> listener = mock(Listener.class);
        eventStudio().add(ToggleNewsPanelRequest.class, listener);
        robot.clickOn(victim);
        verify(listener).onEvent(any());
    }

    @Test
    public void onLatestNews() {
        var event = new LatestNewsResponse(List.of(new NewsData(2, "title", "content", LocalDate.now(), "link", false)),
                true);
        eventStudio().broadcast(event);
        waitForFxEvents();
        assertFalse(victim.getWrapped().isDisabled());
    }

    @Test
    public void onLatestNewsWithNotification() {
        var event = new LatestNewsResponse(List.of(new NewsData(2, "title", "content", LocalDate.now(), "link", false)),
                false);
        eventStudio().broadcast(event);
        waitForFxEvents();
        assertFalse(victim.getWrapped().isDisabled());
        FxAssert.verifyThat(".notification", NodeMatchers.isVisible());
    }

    @Test
    public void onLatestNewsNoNews() {
        var event = new LatestNewsResponse(List.of(), true);
        eventStudio().broadcast(event);
        waitForFxEvents();
        assertTrue(victim.getWrapped().isDisabled());
    }

}