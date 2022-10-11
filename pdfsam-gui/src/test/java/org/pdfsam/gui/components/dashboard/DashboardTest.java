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

import javafx.scene.Node;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.gui.components.dashboard.about.AboutDashboardPane;
import org.pdfsam.injector.Components;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.model.ui.SetActiveDashboardItemRequest;
import org.pdfsam.model.ui.SetTitleRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class, ClearEventStudioExtension.class })
public class DashboardTest {

    @Components({ AboutDashboardItem.class })
    static class Config {
        @Provides
        public AboutDashboardPane aboutPane() {
            AppBrand appBrand = mock(AppBrand.class);
            when(appBrand.property(BrandableProperty.SHORT_NAME)).thenReturn("PDFsam");
            when(appBrand.property(BrandableProperty.NAME)).thenReturn("PDF Split And Merge");
            when(appBrand.property(BrandableProperty.VERSION)).thenReturn("3.0.0");
            when(appBrand.property(BrandableProperty.HOME_URL)).thenReturn("http://www.pdfsam.org");
            when(appBrand.property(BrandableProperty.HOME_LABEL)).thenReturn("home");
            when(appBrand.property(BrandableProperty.FEED_URL)).thenReturn("http://www.pdfsam.org/feed/");
            when(appBrand.property(BrandableProperty.DOCUMENTATION_URL)).thenReturn(
                    "http://www.pdfsam.org/documentation");
            when(appBrand.property(BrandableProperty.SUPPORT_URL)).thenReturn("http://www.pdfsam.org/support");
            when(appBrand.property(BrandableProperty.SCM_URL)).thenReturn("http://www.pdfsam.org/scm");
            when(appBrand.property(BrandableProperty.TRANSLATE_URL)).thenReturn("http://www.pdfsam.org/translate");
            when(appBrand.property(BrandableProperty.TWITTER_URL)).thenReturn("http://www.pdfsam.org/twitter");
            when(appBrand.property(BrandableProperty.DONATE_URL)).thenReturn("http://www.pdfsam.org/donate");
            when(appBrand.property(BrandableProperty.FACEBOOK_URL)).thenReturn("http://www.pdfsam.org/facebook");
            when(appBrand.property(BrandableProperty.LICENSE_NAME)).thenReturn("agpl3");
            when(appBrand.property(BrandableProperty.LICENSE_URL)).thenReturn(
                    "http://www.gnu.org/licenses/agpl-3.0.html");
            when(appBrand.property(BrandableProperty.TRACKER_URL)).thenReturn("http://www.pdfsam.org/issue_tracker");
            when(appBrand.property(BrandableProperty.THANKS_URL)).thenReturn("http://www.pdfsam.org/issue_tracker");
            AboutDashboardPane about = new AboutDashboardPane(appBrand);
            about.setId("aboutPane");
            return about;
        }

    }

    static class DisabledItem implements DashboardItem {

        @Override
        public int priority() {
            return 0;
        }

        @Override
        public Pane pane() {
            return new HBox();
        }

        @Override
        public String name() {
            return "disabled";
        }

        @Override
        public String id() {
            return "disabled";
        }

        @Override
        public Node graphic() {
            return null;
        }

        @Override
        public boolean disabled() {
            return true;
        }
    }

    static class EnabledItem implements DashboardItem {

        @Override
        public int priority() {
            return 0;
        }

        @Override
        public Pane pane() {
            return new HBox();
        }

        @Override
        public String name() {
            return "enabled";
        }

        @Override
        public String id() {
            return "enabled";
        }

        @Override
        public Node graphic() {
            return null;
        }
    }

    @Test
    public void allItemsArePicked() {
        Injector.addConfig(new Config());
        Injector.add(EnabledItem.class);
        try (Injector injector = Injector.start()) {
            Dashboard victim = injector.instance(Dashboard.class);
            assertEquals(2, victim.getChildren().size());
        }
    }

    @Test
    public void disabledItemsAreNotPicked() {
        List<DashboardItem> items = Arrays.asList(new EnabledItem(), new DisabledItem());
        Dashboard victim = new Dashboard(items, new QuickbarDashboardButtonsPane(items));
        assertTrue(victim.hasItem("enabled"));
        assertFalse(victim.hasItem("disabled"));
    }

    @Test
    public void wrongModuleDoesntBoom() {
        try (Injector injector = Injector.start(new Config())) {
            Dashboard victim = injector.instance(Dashboard.class);
            victim.onSetActiveDashboardItem(new SetActiveDashboardItemRequest("chuck norris"));
        }
    }

    @Test
    public void eventIsSent() {
        try (Injector injector = Injector.start(new Config())) {
            Dashboard victim = injector.instance(Dashboard.class);
            assertTrue(((StackPane) victim.getCenter()).getChildren().isEmpty());
            Listener<SetTitleRequest> listener = mock(Listener.class);
            eventStudio().add(SetTitleRequest.class, listener);
            victim.onSetActiveDashboardItem(
                    new SetActiveDashboardItemRequest(injector.instance(AboutDashboardItem.class).id()));
            verify(listener).onEvent(any());
            assertFalse(((StackPane) victim.getCenter()).getChildren().isEmpty());
        }
    }

}
