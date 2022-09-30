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
package org.pdfsam.ui.components.dashboard.about;

import javafx.scene.Scene;
import javafx.scene.input.Clipboard;
import javafx.stage.Stage;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.core.AppBrand;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.NativeOpenUrlRequest;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
public class AboutDashboardPaneTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();
    private Listener<NativeOpenUrlRequest> listener;
    private ArgumentCaptor<NativeOpenUrlRequest> captor;

    @Before
    public void setUp() {
        listener = mock(Listener.class);
        eventStudio().add(NativeOpenUrlRequest.class, listener);
        captor = ArgumentCaptor.forClass(NativeOpenUrlRequest.class);
    }

    @Override
    public void start(Stage stage) {
        AppBrand appBrand = mock(AppBrand.class);
        when(appBrand.name()).thenReturn("PDFsam");
        when(appBrand.property(BrandableProperty.VERSION)).thenReturn("3.0.0");
        when(appBrand.property(BrandableProperty.HOME_URL)).thenReturn("https://www.pdfsam.org");
        when(appBrand.property(BrandableProperty.HOME_LABEL)).thenReturn("home");
        when(appBrand.property(BrandableProperty.FEED_URL)).thenReturn("https://www.pdfsam.org/feed/");
        when(appBrand.property(BrandableProperty.DOCUMENTATION_URL)).thenReturn("https://www.pdfsam.org/documentation");
        when(appBrand.property(BrandableProperty.SUPPORT_URL)).thenReturn("https://www.pdfsam.org/support");
        when(appBrand.property(BrandableProperty.SCM_URL)).thenReturn("https://www.pdfsam.org/scm");
        when(appBrand.property(BrandableProperty.TRANSLATE_URL)).thenReturn("https://www.pdfsam.org/translate");
        when(appBrand.property(BrandableProperty.TWITTER_URL)).thenReturn("https://www.pdfsam.org/twitter");
        when(appBrand.property(BrandableProperty.DONATE_URL)).thenReturn("https://www.pdfsam.org/donate");
        when(appBrand.property(BrandableProperty.FACEBOOK_URL)).thenReturn("https://www.pdfsam.org/facebook");
        when(appBrand.property(BrandableProperty.LICENSE_NAME)).thenReturn("agpl3");
        when(appBrand.property(BrandableProperty.LICENSE_URL)).thenReturn("http://www.gnu.org/licenses/agpl-3.0.html");
        when(appBrand.property(BrandableProperty.TRACKER_URL)).thenReturn("https://www.pdfsam.org/issue_tracker");
        when(appBrand.property(BrandableProperty.THANKS_URL)).thenReturn("https://www.pdfsam.org/thanks_to");
        Scene scene = new Scene(new AboutDashboardPane(appBrand));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void clickHome() {
        clickOn("home");
        verify(listener).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org", captor.getValue().getUrl());
    }

    @Test
    public void clickFeed() {
        clickOn(i18n().tr("Subscribe to the official news feed"));
        verify(listener).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/feed/", captor.getValue().getUrl());
    }

    @Test
    public void clickBugs() {
        clickOn(i18n().tr("Bug and feature requests"));
        verify(listener).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/issue_tracker", captor.getValue().getUrl());
    }

    @Test
    public void clickLicense() {
        clickOn("agpl3");
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.gnu.org/licenses/agpl-3.0.html", captor.getValue().getUrl());
    }

    @Test
    public void clickDocumentation() {
        clickOn(i18n().tr("Documentation"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/documentation", captor.getValue().getUrl());
    }

    @Test
    public void clickScm() {
        clickOn(i18n().tr("Fork PDFsam on GitHub"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/scm", captor.getValue().getUrl());
    }

    @Test
    public void clickTranslate() {
        clickOn(i18n().tr("Translate"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/translate", captor.getValue().getUrl());
    }

    @Test
    public void clickDonate() {
        clickOn(i18n().tr("Donate"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/donate", captor.getValue().getUrl());
    }

    @Test
    public void clickTwitter() {
        clickOn(i18n().tr("Follow us on Twitter"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/twitter", captor.getValue().getUrl());
    }

    @Test
    public void clickFacebook() {
        clickOn(i18n().tr("Like us on Facebook"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/facebook", captor.getValue().getUrl());
    }

    @Test
    public void clickCopy() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            Clipboard.getSystemClipboard().clear();
            assertTrue(isBlank(Clipboard.getSystemClipboard().getString()));
        });

        clickOn("#copyEnvDetails");
        WaitForAsyncUtils.waitForAsyncFx(2000,
                () -> assertTrue(Clipboard.getSystemClipboard().getString().contains("PDFsam")));
    }
}
