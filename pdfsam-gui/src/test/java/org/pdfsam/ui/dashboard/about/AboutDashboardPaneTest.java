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
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.Parent;
import javafx.scene.input.Clipboard;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.mockito.ArgumentCaptor;
import org.pdfsam.Pdfsam;
import org.pdfsam.PdfsamEdition;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.OpenUrlRequest;
import org.sejda.eventstudio.Listener;

import de.jensd.fx.fontawesome.AwesomeIcon;

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
        return new AboutDashboardPane(new Pdfsam(PdfsamEdition.COMMUNITY, "PDFsam", "3.0.0"));
    }

    @Test
    public void clickHome() {
        click(AwesomeIcon.HOME.toString());
        verify(listener).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org", captor.getValue().getUrl());
    }

    @Test
    public void clickFeed() {
        click(AwesomeIcon.RSS_SQUARE.toString());
        verify(listener).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/feed/", captor.getValue().getUrl());
    }

    @Test
    public void clickBugs() {
        click(AwesomeIcon.BUG.toString());
        verify(listener).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/issue_tracker", captor.getValue().getUrl());
    }

    @Test
    public void clickWiki() {
        click(AwesomeIcon.QUESTION_CIRCLE.toString());
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/wiki", captor.getValue().getUrl());
    }

    @Test
    public void clickQuickstart() {
        click(AwesomeIcon.YOUTUBE_PLAY.toString());
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/quickstart_video", captor.getValue().getUrl());
    }

    @Test
    public void clickScm() {
        click(AwesomeIcon.GITHUB.toString());
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/scm", captor.getValue().getUrl());
    }

    @Test
    public void clickTranslate() {
        click(AwesomeIcon.FLAG_ALT.toString());
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/translate", captor.getValue().getUrl());
    }

    @Test
    public void clickDonate() {
        click(AwesomeIcon.DOLLAR.toString());
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/donate", captor.getValue().getUrl());
    }

    @Test
    public void clickTwitter() {
        click(AwesomeIcon.TWITTER_SQUARE.toString());
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/twitter", captor.getValue().getUrl());
    }

    @Test
    public void clickGPlus() {
        click(AwesomeIcon.GOOGLE_PLUS_SQUARE.toString());
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/gplus", captor.getValue().getUrl());
    }

    @Test
    public void clickFacebook() {
        click(AwesomeIcon.FACEBOOK_SQUARE.toString());
        verify(listener, atLeastOnce()).onEvent(captor.capture());
        assertEquals("http://www.pdfsam.org/facebook", captor.getValue().getUrl());
    }

    @Test
    public void clickCopy() throws Exception {
        FXTestUtils.invokeAndWait(() -> {
            Clipboard.getSystemClipboard().clear();
            assertTrue(isBlank(Clipboard.getSystemClipboard().getString()));
        }, 2);
        click("#copyEnvDetails");
        FXTestUtils.invokeAndWait(() -> assertTrue(Clipboard.getSystemClipboard().getString().contains("PDFsam")), 1);
    }
}
