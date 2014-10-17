/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/set/2014
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
package org.pdfsam.ui.dashboard;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.Parent;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.mockito.ArgumentCaptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityDashboardItem;
import org.pdfsam.ui.event.SetActiveDashboardItemRequest;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class DashboardButtonTest extends GuiTest {

    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();

    @Override
    protected Parent getRootNode() {
        return new DashboardButton(new DefaultPriorityDashboardItem());
    }

    @Test
    public void onClick() {
        Listener<SetActiveDashboardItemRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveDashboardItemRequest.class, listener);
        click(".quickbar-navigation-button");
        ArgumentCaptor<SetActiveDashboardItemRequest> captor = ArgumentCaptor
                .forClass(SetActiveDashboardItemRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(DefaultPriorityDashboardItem.ID, captor.getValue().getActiveItemId());
    }

    @Test
    public void selectIf() throws Exception {
        DashboardButton victim = find(".quickbar-navigation-button");
        assertFalse(victim.isSelected());
        victim.selectIf("ImNotMatching");
        assertFalse(victim.isSelected());
        FXTestUtils.invokeAndWait(() -> victim.selectIf(DefaultPriorityDashboardItem.ID), 1);
        assertTrue(victim.isSelected());
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArg() {
        new DashboardButton(null);
    }
}
