/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/set/2014
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
package org.pdfsam.ui.components;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import javax.inject.Named;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.tool.Tool;
import org.pdfsam.service.tool.UsageService;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityDashboardItem;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.model.ui.SetActiveToolRequest;
import org.pdfsam.ui.components.event.SetActiveDashboardItemRequest;
import org.pdfsam.ui.components.dashboard.Dashboard;
import org.pdfsam.ui.components.dashboard.DashboardItem;
import org.pdfsam.ui.components.workarea.WorkArea;

/**
 * @author Andrea Vacondio
 *
 */
public class ContentPaneTest {
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private Injector injector;

    @Before
    public void setUp() {
        injector = Injector.start(new Config());
    }

    static class Config {
        @Provides
        public UsageService usageService() {
            return mock(UsageService.class);
        }

        @Provides
        public Tool module() {
            return new DefaultPriorityTestTool();
        }

        @Provides
        public DashboardItem dashboardItem() {
            return new DefaultPriorityDashboardItem();
        }

        @Provides
        @Named("defaultDashboardItemId")
        public String defaultItem() {
            return "defaultItem";
        }
    }

    @Test
    public void onSetActiveModule() {
        injector.instance(ContentPane.class).onSetActiveModule(SetActiveToolRequest.activeteCurrentModule());
        assertTrue(injector.instance(WorkArea.class).isVisible());
        assertFalse(injector.instance(Dashboard.class).isVisible());
    }

    @Test
    public void onSetActiveDashboardItem() {
        injector.instance(ContentPane.class).onSetActiveDashboardItem(new SetActiveDashboardItemRequest("id"));
        assertTrue(injector.instance(Dashboard.class).isVisible());
        assertFalse(injector.instance(WorkArea.class).isVisible());
    }

}
