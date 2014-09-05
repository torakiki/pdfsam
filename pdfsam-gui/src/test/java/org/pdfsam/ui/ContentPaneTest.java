/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/set/2014
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
package org.pdfsam.ui;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityDashboardItem;
import org.pdfsam.test.DefaultPriorityTestModule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.dashboard.Dashboard;
import org.pdfsam.ui.event.SetActiveDashboardItemRequest;
import org.pdfsam.ui.event.SetActiveModuleRequest;
import org.pdfsam.ui.workarea.WorkArea;

/**
 * @author Andrea Vacondio
 *
 */
public class ContentPaneTest {
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private WorkArea modules;
    private Dashboard dashboard;
    private ContentPane victim;

    @Before
    public void setUp() {
        modules = new WorkArea(Arrays.asList(new DefaultPriorityTestModule()));
        dashboard = new Dashboard(Arrays.asList(new DefaultPriorityDashboardItem()));
        victim = new ContentPane(modules, dashboard, "defaultItem");
    }

    @Test
    public void onSetActiveModule() {
        victim.onSetActiveModule(SetActiveModuleRequest.activeteCurrentModule());
        assertTrue(modules.isVisible());
        assertFalse(dashboard.isVisible());
    }

    @Test
    public void onSetActiveDashboardItem() {
        victim.onSetActiveDashboardItem(new SetActiveDashboardItemRequest("id"));
        assertTrue(dashboard.isVisible());
        assertFalse(modules.isVisible());
    }

}
