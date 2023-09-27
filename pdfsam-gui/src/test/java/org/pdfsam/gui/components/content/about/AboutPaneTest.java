/*
 * This file is part of the PDF Split And Merge source code
 * Created on 03/set/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.content.about;

import javafx.scene.Scene;
import javafx.scene.input.Clipboard;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.io.NativeOpenUrlRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class AboutPaneTest {

    private Listener<NativeOpenUrlRequest> listener;
    private ArgumentCaptor<NativeOpenUrlRequest> captor;
    private FxRobot robot;

    @BeforeEach
    public void setUp() {
        listener = mock(Listener.class);
        eventStudio().add(NativeOpenUrlRequest.class, listener);
        captor = ArgumentCaptor.forClass(NativeOpenUrlRequest.class);
    }

    @Start
    public void start(Stage stage) {
        AppBrand appBrand = mock(AppBrand.class);
        when(appBrand.property(BrandableProperty.SHORT_NAME)).thenReturn("PDFsam");
        when(appBrand.property(eq(BrandableProperty.NAME), any())).thenReturn("PDF Split And Merge");
        when(appBrand.property(BrandableProperty.VERSION)).thenReturn("3.0.0");
        when(appBrand.property(BrandableProperty.HOME_URL)).thenReturn("https://www.pdfsam.org");
        when(appBrand.property(BrandableProperty.HOME_LABEL)).thenReturn("home");
        when(appBrand.property(BrandableProperty.VENDOR_URL)).thenReturn("https://soberlemur.com");
        when(appBrand.property(BrandableProperty.COPYRIGHT)).thenReturn("Sober Lemur S.r.l.");
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
        Scene scene = new Scene(new AboutPane(appBrand));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void clickVendor() {
        robot.clickOn("Sober Lemur S.r.l.");
        verify(listener).onEvent(captor.capture());
        assertEquals("https://soberlemur.com", captor.getValue().url());
    }

    @Test
    public void clickHome() {
        robot.clickOn("home");
        verify(listener).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org", captor.getValue().url());
    }

    @Test
    public void clickFeed() {
        robot.clickOn(i18n().tr("Subscribe to the official news feed"));
        verify(listener).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/feed/", captor.getValue().url());
    }

    @Test
    public void clickBugs() {
        robot.clickOn(i18n().tr("Bug and feature requests"));
        verify(listener).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/issue_tracker", captor.getValue().url());
    }

    @Test
    public void clickLicense() {
        robot.clickOn("agpl3");
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.gnu.org/licenses/agpl-3.0.html", captor.getValue().url());
    }

    @Test
    public void clickDocumentation() {
        robot.clickOn(i18n().tr("Documentation"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/documentation", captor.getValue().url());
    }

    @Test
    public void clickScm() {
        robot.clickOn(i18n().tr("Fork PDFsam on GitHub"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/scm", captor.getValue().url());
    }

    @Test
    public void clickTranslate() {
        robot.clickOn(i18n().tr("Translate"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/translate", captor.getValue().url());
    }

    @Test
    public void clickDonate() {
        robot.clickOn(i18n().tr("Donate"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/donate", captor.getValue().url());
    }

    @Test
    public void clickTwitter() {
        robot.clickOn(i18n().tr("Follow us on Twitter"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/twitter", captor.getValue().url());
    }

    @Test
    public void clickFacebook() {
        robot.clickOn(i18n().tr("Like us on Facebook"));
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("https://www.pdfsam.org/facebook", captor.getValue().url());
    }

    @Test
    public void clickCopy() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            Clipboard.getSystemClipboard().clear();
            assertTrue(isBlank(Clipboard.getSystemClipboard().getString()));
        });

        robot.clickOn("#copyEnvDetails");
        WaitForAsyncUtils.waitForAsyncFx(2000,
                () -> assertThat(Clipboard.getSystemClipboard().getString()).contains("PDF Split And Merge"));
    }
}
