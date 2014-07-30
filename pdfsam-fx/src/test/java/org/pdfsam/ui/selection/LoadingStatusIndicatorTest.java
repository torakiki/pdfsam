/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/lug/2014
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
package org.pdfsam.ui.selection;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.Parent;
import javafx.scene.input.KeyCode;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.runner.RunWith;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfDocumentDescriptorProvider;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.commons.ShowStageRequest;

import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(MockitoJUnitRunner.class)
@Category(TestFX.class)
public class LoadingStatusIndicatorTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule("LogStage");

    @Mock
    private PdfDocumentDescriptorProvider provider;
    @Mock
    private PdfDocumentDescriptor descriptor;

    @Before
    public void setUp() {
        when(provider.getPdfDocumentDescriptor()).thenReturn(descriptor);
    }

    @Override
    protected Parent getRootNode() {
        return new LoadingStatusIndicator(provider, "module");
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArg() {
        new LoadingStatusIndicator(null, "module");
    }

    @Test
    public void initial() {
        click(".encryption-status");
        assertEquals(1, getWindows().size());
    }

    @Test
    public void openLogStageOnError() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(() -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.WITH_ERRORS), 2);
        HitTestListener<ShowStageRequest> listener = new HitTestListener<>();
        eventStudio().add(ShowStageRequest.class, listener, "LogStage");
        click(victim);
        assertTrue(listener.isHit());
        exists(AwesomeIcon.WARNING.toString());
    }

    @Test
    public void gearOnRequested() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(() -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.REQUESTED), 2);
        exists(AwesomeIcon.GEAR.toString());
    }

    @Test
    public void clickOnRequested() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(() -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.REQUESTED), 2);
        click(victim);
        assertEquals(1, getWindows().size());
    }

    @Test
    public void gearOnLoading() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(() -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.LOADING), 2);
        exists(AwesomeIcon.GEAR.toString());
    }

    @Test
    public void clickOnLoading() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(() -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.LOADING), 2);
        click(victim);
        assertEquals(1, getWindows().size());
    }

    @Test
    public void unlockOnLoadedWithPwd() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(
                () -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION), 2);
        exists(AwesomeIcon.UNLOCK.toString());
    }

    @Test
    public void clickOnLoadedWithPwd() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(
                () -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION), 2);
        click(victim);
        assertEquals(1, getWindows().size());
    }

    @Test
    public void lockOnEncrypted() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(() -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.ENCRYPTED), 2);
        exists(AwesomeIcon.LOCK.toString());
    }

    @Test
    public void clickOnEncryptedShowPwdField() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(() -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.ENCRYPTED), 2);
        click(victim);
        assertEquals(2, getWindows().size());
    }

    @Test
    public void pwdSentOnEnterKey() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(() -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.ENCRYPTED), 2);
        click(victim);
        HitTestListener<PdfLoadRequestEvent> listener = new HitTestListener<>();
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        type("myPwd").type(KeyCode.ENTER);
        assertTrue(listener.isHit());
        verify(descriptor).setPassword("myPwd");
    }

    @Test
    public void pwdSentOnButtonPressed() throws Exception {
        LoadingStatusIndicator victim = find(".encryption-status");
        FXTestUtils.invokeAndWait(() -> victim.setLoadingStatus(PdfDescriptorLoadingStatus.ENCRYPTED), 2);
        click(victim);
        HitTestListener<PdfLoadRequestEvent> listener = new HitTestListener<>();
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        type("myPwd").click(".pdfsam-button");
        assertTrue(listener.isHit());
        verify(descriptor).setPassword("myPwd");
    }

}
