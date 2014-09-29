/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/set/2014
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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityDashboardItem;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.event.SetActiveModuleRequest;

/**
 * @author Andrea Vacondio
 *
 */
public class DashboardItemPaneTest {

    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();

    @Test
    public void footerIsAdded() {
        DashboardItemPane victim = new DashboardItemPane(new DefaultPriorityDashboardItem());
        assertNull(victim.lookup(".pdfsam-button"));
        eventStudio().broadcast(SetActiveModuleRequest.activeteModule("any"));
        assertNotNull(victim.lookup(".pdfsam-button"));
    }
}
