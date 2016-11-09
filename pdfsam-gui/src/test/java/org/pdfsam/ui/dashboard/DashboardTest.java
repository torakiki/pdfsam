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
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Arrays;
import java.util.List;

import javax.inject.Inject;

import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.pdfsam.ConfigurableProperty;
import org.pdfsam.Pdfsam;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.dashboard.about.AboutDashboardPane;
import org.pdfsam.ui.event.SetActiveDashboardItemRequest;
import org.pdfsam.ui.event.SetTitleEvent;
import org.sejda.eventstudio.Listener;

import javafx.scene.layout.StackPane;

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
            Pdfsam pdfsam = mock(Pdfsam.class);
            when(pdfsam.name()).thenReturn("PDFsam");
            when(pdfsam.property(ConfigurableProperty.VERSION)).thenReturn("3.0.0");
            when(pdfsam.property(ConfigurableProperty.HOME_URL)).thenReturn("http://www.pdfsam.org");
            when(pdfsam.property(ConfigurableProperty.HOME_LABEL)).thenReturn("home");
            when(pdfsam.property(ConfigurableProperty.FEED_URL)).thenReturn("http://www.pdfsam.org/feed/");
            when(pdfsam.property(ConfigurableProperty.QUICKSTART_URL))
                    .thenReturn("http://www.pdfsam.org/quickstart_video");
            when(pdfsam.property(ConfigurableProperty.SUPPORT_URL)).thenReturn("http://www.pdfsam.org/support");
            when(pdfsam.property(ConfigurableProperty.SCM_URL)).thenReturn("http://www.pdfsam.org/scm");
            when(pdfsam.property(ConfigurableProperty.TRANSLATE_URL)).thenReturn("http://www.pdfsam.org/translate");
            when(pdfsam.property(ConfigurableProperty.TWITTER_URL)).thenReturn("http://www.pdfsam.org/twitter");
            when(pdfsam.property(ConfigurableProperty.DONATE_URL)).thenReturn("http://www.pdfsam.org/donate");
            when(pdfsam.property(ConfigurableProperty.GPLUS_URL)).thenReturn("http://www.pdfsam.org/gplus");
            when(pdfsam.property(ConfigurableProperty.FACEBOOK_URL)).thenReturn("http://www.pdfsam.org/facebook");
            when(pdfsam.property(ConfigurableProperty.LICENSE_NAME)).thenReturn("agpl3");
            when(pdfsam.property(ConfigurableProperty.LICENSE_URL))
                    .thenReturn("http://www.gnu.org/licenses/agpl-3.0.html");
            when(pdfsam.property(ConfigurableProperty.TRACKER_URL)).thenReturn("http://www.pdfsam.org/issue_tracker");
            when(pdfsam.property(ConfigurableProperty.THANKS_URL)).thenReturn("http://www.pdfsam.org/issue_tracker");
            when(pdfsam.property(ConfigurableProperty.GPLUS_URL)).thenReturn("http://www.pdfsam.org/gplus");
            AboutDashboardPane about = new AboutDashboardPane(pdfsam);
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
            return new Dashboard(items());
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
