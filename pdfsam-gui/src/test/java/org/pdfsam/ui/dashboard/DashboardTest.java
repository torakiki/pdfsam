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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Arrays;
import java.util.List;

import javafx.scene.layout.StackPane;

import javax.inject.Inject;

import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.dashboard.about.AboutDashboardPane;
import org.pdfsam.ui.event.SetActiveDashboardItemRequest;
import org.pdfsam.ui.event.SetTitleEvent;
import org.sejda.eventstudio.Listener;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class DashboardTest {
    @ClassRule
    public static ClearEventStudioRule STUDIO_RULE = new ClearEventStudioRule();
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    @Inject
    private ApplicationContext applicationContext;

    @Configuration
    @Lazy
    static class Config {
        @Bean
        public AboutDashboardPane aboutPane() {
            AboutDashboardPane about = new AboutDashboardPane("PDFsam", "version");
            about.setId("aboutPane");
            return about;
        }

        @Bean
        public AboutDashboadItem item() {
            return new AboutDashboadItem(aboutPane());
        }

        @Bean
        public List<DashboardItem> items() {
            return Arrays.asList(item());
        }

        @Bean
        public QuickbarDashboardButtonsPane buttons() {
            return new QuickbarDashboardButtonsPane(items());
        }

        @Bean
        public Dashboard victim() {
            return new Dashboard(items(), buttons());
        }
    }

    @Test
    public void wrongModuleDoesntBoom() {
        Dashboard victim = applicationContext.getBean(Dashboard.class);
        victim.onSetActiveDashboardItem(new SetActiveDashboardItemRequest("chuck norris"));
    }

    @Test
    public void eventIsSent() {
        Dashboard victim = applicationContext.getBean(Dashboard.class);
        assertTrue(((StackPane) victim.getCenter()).getChildren().isEmpty());
        Listener<SetTitleEvent> listener = mock(Listener.class);
        eventStudio().add(SetTitleEvent.class, listener);
        victim.onSetActiveDashboardItem(new SetActiveDashboardItemRequest(applicationContext.getBean(
                AboutDashboadItem.class).id()));
        verify(listener).onEvent(any());
        assertFalse(((StackPane) victim.getCenter()).getChildren().isEmpty());
    }

}
