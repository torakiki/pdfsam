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
package org.pdfsam.ui.dashboard.about;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.pdfsam.ConfigurableProperty;
import org.pdfsam.Pdfsam;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.OpenUrlRequest;
import org.sejda.eventstudio.Listener;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.input.Clipboard;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class AboutDashboardPaneTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();
    private Listener<OpenUrlRequest> listener;
    private ArgumentCaptor<OpenUrlRequest> captor;

    @Before
    public void setUp() {
        listener = mock(Listener.class);
        eventStudio().add(OpenUrlRequest.class, listener);
        captor = ArgumentCaptor.forClass(OpenUrlRequest.class);
    }

    @Override
    public void start(Stage stage) {
        Pdfsam pdfsam = mock(Pdfsam.class);
        when(pdfsam.name()).thenReturn("PDFsam");
        when(pdfsam.property(ConfigurableProperty.VERSION)).thenReturn("3.0.0");
        when(pdfsam.property(ConfigurableProperty.HOME_URL)).thenReturn("https://www.pdfsam.org");
        when(pdfsam.property(ConfigurableProperty.HOME_LABEL)).thenReturn("home");
        when(pdfsam.property(ConfigurableProperty.FEED_URL)).thenReturn("https://www.pdfsam.org/feed/");
        when(pdfsam.property(ConfigurableProperty.DOCUMENTATION_URL))
                .thenReturn("https://www.pdfsam.org/documentation");
        when(pdfsam.property(ConfigurableProperty.SUPPORT_URL)).thenReturn("https://www.pdfsam.org/support");
        when(pdfsam.property(ConfigurableProperty.SCM_URL)).thenReturn("https://www.pdfsam.org/scm");
        when(pdfsam.property(ConfigurableProperty.TRANSLATE_URL)).thenReturn("https://www.pdfsam.org/translate");
        when(pdfsam.property(ConfigurableProperty.TWITTER_URL)).thenReturn("https://www.pdfsam.org/twitter");
        when(pdfsam.property(ConfigurableProperty.DONATE_URL)).thenReturn("https://www.pdfsam.org/donate");
        when(pdfsam.property(ConfigurableProperty.FACEBOOK_URL)).thenReturn("https://www.pdfsam.org/facebook");
        when(pdfsam.property(ConfigurableProperty.LICENSE_NAME)).thenReturn("agpl3");
        when(pdfsam.property(ConfigurableProperty.LICENSE_URL)).thenReturn("http://www.gnu.org/licenses/agpl-3.0.html");
        when(pdfsam.property(ConfigurableProperty.TRACKER_URL)).thenReturn("https://www.pdfsam.org/issue_tracker");
        when(pdfsam.property(ConfigurableProperty.THANKS_URL)).thenReturn("https://www.pdfsam.org/thanks_to");
        Scene scene = new Scene(new AboutDashboardPane(pdfsam));
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
        clickOn(DefaultI18nContext.getInstance().i18n("Subscribe to the official news feed"));
        verify(listener).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/feed/", captor.getValue().getUrl());
    }

    @Test
    public void clickBugs() {
        clickOn(DefaultI18nContext.getInstance().i18n("Bug and feature requests"));
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
        clickOn(DefaultI18nContext.getInstance().i18n("Documentation"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/documentation", captor.getValue().getUrl());
    }

    @Test
    public void clickScm() {
        clickOn(DefaultI18nContext.getInstance().i18n("Fork PDFsam on GitHub"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/scm", captor.getValue().getUrl());
    }

    @Test
    public void clickTranslate() {
        clickOn(DefaultI18nContext.getInstance().i18n("Translate"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/translate", captor.getValue().getUrl());
    }

    @Test
    public void clickDonate() {
        clickOn(DefaultI18nContext.getInstance().i18n("Donate"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/donate", captor.getValue().getUrl());
    }

    @Test
    public void clickTwitter() {
        clickOn(DefaultI18nContext.getInstance().i18n("Follow us on Twitter"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/twitter", captor.getValue().getUrl());
    }

    @Test
    public void clickGPlus() {
        clickOn(DefaultI18nContext.getInstance().i18n("Follow us on Google Plus"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/gplus", captor.getValue().getUrl());
    }

    @Test
    public void clickFacebook() {
        clickOn(DefaultI18nContext.getInstance().i18n("Like us on Facebook"));
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
