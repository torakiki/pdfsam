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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.ui.SetActiveDashboardItemRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadExtension;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class, ClearEventStudioExtension.class })
public class QuickbarDashboardButtonsPaneTest {

    private QuickbarDashboardButtonsPane victim;

    @BeforeEach
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
        assertTrue(victim.getChildren().stream().filter(n -> n instanceof DashboardButton).map(n -> (DashboardButton) n)
                .allMatch(DashboardButton::isDisplayText));
        victim.setDisplayText(false);
        assertTrue(victim.getChildren().stream().filter(n -> n instanceof DashboardButton).map(n -> (DashboardButton) n)
                .noneMatch(DashboardButton::isDisplayText));
    }

    @Test
    public void select() {
        assertTrue(victim.getChildren().stream().filter(n -> n instanceof DashboardButton).map(n -> (DashboardButton) n)
                .noneMatch(DashboardButton::isSelected));
        victim.setCurrentDashboardItem(new SetActiveDashboardItemRequest("id1"));
        long selected = victim.getChildren().stream().filter(n -> n instanceof DashboardButton)
                .map(n -> (DashboardButton) n).filter(DashboardButton::isSelected).count();
        assertEquals(1, selected);
    }

}
