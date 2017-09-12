/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.event.SetActiveDashboardItemRequest;

/**
 * @author Andrea Vacondio
 *
 */
public class QuickbarDashboardButtonsPaneTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    private QuickbarDashboardButtonsPane victim;

    @Before
    public void setUp() {
        DashboardItem item1 = mock(DashboardItem.class);
        when(item1.name()).thenReturn("name1");
        when(item1.id()).thenReturn("id1");
        DashboardItem item2 = mock(DashboardItem.class);
        when(item2.name()).thenReturn("name2");
        when(item2.id()).thenReturn("id2");
        victim = new QuickbarDashboardButtonsPane(Arrays.asList(item1, item2));
    }

    @Test
    public void displayText() {
        victim.getChildren().stream().filter(n -> n instanceof DashboardButton).map(n -> (DashboardButton) n)
                .noneMatch(DashboardButton::isDisplayText);
        victim.setDisplayText(true);
        victim.getChildren().stream().filter(n -> n instanceof DashboardButton).map(n -> (DashboardButton) n)
                .allMatch(DashboardButton::isDisplayText);
    }

    @Test
    public void select() {
        victim.getChildren().stream().filter(n -> n instanceof DashboardButton).map(n -> (DashboardButton) n)
                .noneMatch(DashboardButton::isSelected);
        victim.setCurrentDashboardItem(new SetActiveDashboardItemRequest("id1"));
        long selected = victim.getChildren().stream().filter(n -> n instanceof DashboardButton)
                .map(n -> (DashboardButton) n).filter(DashboardButton::isSelected).count();
        assertEquals(1, selected);
    }

}
