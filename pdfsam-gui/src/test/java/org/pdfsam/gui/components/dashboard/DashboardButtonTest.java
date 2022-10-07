/*
 * This file is part of the PDF Split And Merge source code
 * Created on 03/set/2014
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
package org.pdfsam.gui.components.dashboard;

import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.gui.test.DefaultPriorityDashboardItem;
import org.pdfsam.model.ui.SetActiveDashboardItemRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class DashboardButtonTest {
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        Scene scene = new Scene(new HBox(new DashboardButton(new DefaultPriorityDashboardItem())));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void onClick() {
        Listener<SetActiveDashboardItemRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveDashboardItemRequest.class, listener);
        robot.clickOn(".quickbar-navigation-button");
        ArgumentCaptor<SetActiveDashboardItemRequest> captor = ArgumentCaptor.forClass(
                SetActiveDashboardItemRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(DefaultPriorityDashboardItem.ID, captor.getValue().id());
    }

    @Test
    public void selectIf() {
        DashboardButton victim = robot.lookup(".quickbar-navigation-button").queryAs(DashboardButton.class);
        assertFalse(victim.isSelected());
        victim.selectIf("ImNotMatching");
        assertFalse(victim.isSelected());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.selectIf(DefaultPriorityDashboardItem.ID));
        assertTrue(victim.isSelected());
    }

    @Test
    public void nullArg() {
        assertThrows(IllegalArgumentException.class, () -> new DashboardButton(null));
    }
}
