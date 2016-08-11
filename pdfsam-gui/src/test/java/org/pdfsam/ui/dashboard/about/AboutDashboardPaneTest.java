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
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.mockito.ArgumentCaptor;
import org.pdfsam.ConfigurableProperty;
import org.pdfsam.Pdfsam;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.OpenUrlRequest;
import org.sejda.eventstudio.Listener;

import javafx.scene.Parent;
import javafx.scene.input.Clipboard;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class AboutDashboardPaneTest extends GuiTest {

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
    protected Parent getRootNode() {
        Pdfsam pdfsam = mock(Pdfsam.class);
        when(pdfsam.name()).thenReturn("PDFsam");
        when(pdfsam.property(ConfigurableProperty.VERSION)).thenReturn("3.0.0");
        when(pdfsam.property(ConfigurableProperty.HOME_URL)).thenReturn("http://www.pdfsam.org");
        when(pdfsam.property(ConfigurableProperty.HOME_LABEL)).thenReturn("home");
        when(pdfsam.property(ConfigurableProperty.FEED_URL)).thenReturn("http://www.pdfsam.org/feed/");
        when(pdfsam.property(ConfigurableProperty.QUICKSTART_URL)).thenReturn("http://www.pdfsam.org/quickstart_video");
        when(pdfsam.property(ConfigurableProperty.SUPPORT_URL)).thenReturn("http://www.pdfsam.org/support");
        when(pdfsam.property(ConfigurableProperty.SCM_URL)).thenReturn("http://www.pdfsam.org/scm");
        when(pdfsam.property(ConfigurableProperty.TRANSLATE_URL)).thenReturn("http://www.pdfsam.org/translate");
        when(pdfsam.property(ConfigurableProperty.TWITTER_URL)).thenReturn("http://www.pdfsam.org/twitter");
        when(pdfsam.property(ConfigurableProperty.DONATE_URL)).thenReturn("http://www.pdfsam.org/donate");
        when(pdfsam.property(ConfigurableProperty.GPLUS_URL)).thenReturn("http://www.pdfsam.org/gplus");
        when(pdfsam.property(ConfigurableProperty.FACEBOOK_URL)).thenReturn("http://www.pdfsam.org/facebook");
        when(pdfsam.property(ConfigurableProperty.LICENSE_NAME)).thenReturn("agpl3");
        when(pdfsam.property(ConfigurableProperty.LICENSE_URL)).thenReturn("http://www.gnu.org/licenses/agpl-3.0.html");
        when(pdfsam.property(ConfigurableProperty.TRACKER_URL)).thenReturn("http://www.pdfsam.org/issue_tracker");
        when(pdfsam.property(ConfigurableProperty.THANKS_URL)).thenReturn("http://www.pdfsam.org/issue_tracker");
        when(pdfsam.property(ConfigurableProperty.GPLUS_URL)).thenReturn("http://www.pdfsam.org/gplus");
        return new AboutDashboardPane(pdfsam);
    }

    @Test
    public void clickHome() {
        click("home");
        verify(listener).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org", captor.getValue().getUrl());
    }

    @Test
    public void clickFeed() {
        click(DefaultI18nContext.getInstance().i18n("Subscribe to the official news feed"));
        verify(listener).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/feed/", captor.getValue().getUrl());
    }

    @Test
    public void clickBugs() {
        click(DefaultI18nContext.getInstance().i18n("Bug and feature requests"));
        verify(listener).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/issue_tracker", captor.getValue().getUrl());
    }

    @Test
    public void clickLicense() {
        click("agpl3");
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.gnu.org/licenses/agpl-3.0.html", captor.getValue().getUrl());
    }

    @Test
    public void clickQuickstart() {
        click(DefaultI18nContext.getInstance().i18n("Play the \"get started\" video"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/quickstart_video", captor.getValue().getUrl());
    }

    @Test
    public void clickScm() {
        click(DefaultI18nContext.getInstance().i18n("Fork PDFsam on GitHub"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/scm", captor.getValue().getUrl());
    }

    @Test
    public void clickTranslate() {
        click(DefaultI18nContext.getInstance().i18n("Translate"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/translate", captor.getValue().getUrl());
    }

    @Test
    public void clickDonate() {
        click(DefaultI18nContext.getInstance().i18n("Donate"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/donate", captor.getValue().getUrl());
    }

    @Test
    public void clickTwitter() {
        click(DefaultI18nContext.getInstance().i18n("Follow us on Twitter"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/twitter", captor.getValue().getUrl());
    }

    @Test
    public void clickGPlus() {
        click(DefaultI18nContext.getInstance().i18n("Follow us on Google Plus"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/gplus", captor.getValue().getUrl());
    }

    @Test
    public void clickFacebook() {
        click(DefaultI18nContext.getInstance().i18n("Like us on Facebook"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/facebook", captor.getValue().getUrl());
    }

    @Test
    public void clickCopy() throws Exception {
        FXTestUtils.invokeAndWait(() -> {
            Clipboard.getSystemClipboard().clear();
            assertTrue(isBlank(Clipboard.getSystemClipboard().getString()));
        } , 2);
        click("#copyEnvDetails");
        FXTestUtils.invokeAndWait(() -> assertTrue(Clipboard.getSystemClipboard().getString().contains("PDFsam")), 1);
    }
}
