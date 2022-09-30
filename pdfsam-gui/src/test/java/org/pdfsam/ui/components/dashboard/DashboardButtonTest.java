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
package org.pdfsam.ui.components.dashboard;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityDashboardItem;
import org.pdfsam.ui.components.event.SetActiveDashboardItemRequest;
import org.pdfsam.eventstudio.Listener;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class DashboardButtonTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();

    @Override
    public void start(Stage stage) {
        Scene scene = new Scene(new HBox(new DashboardButton(new DefaultPriorityDashboardItem())));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void onClick() {
        Listener<SetActiveDashboardItemRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveDashboardItemRequest.class, listener);
        clickOn(".quickbar-navigation-button");
        ArgumentCaptor<SetActiveDashboardItemRequest> captor = ArgumentCaptor
                .forClass(SetActiveDashboardItemRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(DefaultPriorityDashboardItem.ID, captor.getValue().getActiveItemId());
    }

    @Test
    public void selectIf() {
        DashboardButton victim = lookup(".quickbar-navigation-button").queryAs(DashboardButton.class);
        assertFalse(victim.isSelected());
        victim.selectIf("ImNotMatching");
        assertFalse(victim.isSelected());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.selectIf(DefaultPriorityDashboardItem.ID));
        assertTrue(victim.isSelected());
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArg() {
        new DashboardButton(null);
    }
}
