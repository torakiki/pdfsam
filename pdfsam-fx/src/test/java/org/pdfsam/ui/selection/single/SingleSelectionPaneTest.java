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
package org.pdfsam.ui.selection.single;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.hamcrest.Matchers;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.mockito.ArgumentCaptor;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.HitConsumer;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.commons.OpenFileRequest;
import org.pdfsam.ui.commons.SetDestinationRequest;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.io.ChangedSelectedPdfVersionEvent;
import org.sejda.eventstudio.Listener;

import javafx.scene.Parent;
import javafx.scene.control.Label;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.KeyCode;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@SuppressWarnings("unchecked")
public class SingleSelectionPaneTest extends GuiTest {

    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder tmpFolder = new TemporaryFolder();

    @BeforeClass
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
    }

    @Override
    protected Parent getRootNode() {
        SingleSelectionPane victim = new SingleSelectionPane(MODULE);
        victim.setId("victim-selection-pane");
        return victim;
    }

    @Test
    public void loadEvent() throws Exception {
        HitTestListener<PdfLoadRequestEvent> listener = new HitTestListener<>();
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        typePathAndValidate();
        assertTrue(listener.isHit());
    }

    @Test
    public void infoMenuItem() throws Exception {
        HitTestListener<ShowPdfDescriptorRequest> listener = new HitTestListener<>();
        eventStudio().add(ShowPdfDescriptorRequest.class, listener);
        typePathAndValidate();
        rightClick(".validable-container-field");
        click(DefaultI18nContext.getInstance().i18n("Document properties"));
        sleep(500);
        assertTrue(listener.isHit());
    }

    @Test
    public void setDestinationMenuItem() throws Exception {
        HitTestListener<SetDestinationRequest> listener = new HitTestListener<SetDestinationRequest>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertFalse(event.isFallback());
            }
        };
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        typePathAndValidate();
        rightClick(".validable-container-field");
        click(DefaultI18nContext.getInstance().i18n("Set destination"));
        assertTrue(listener.isHit());
    }

    @Test
    public void openFileMenuItem() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        HitTestListener<OpenFileRequest> listener = new HitTestListener<OpenFileRequest>() {
            @Override
            public void onEvent(OpenFileRequest event) {
                super.onEvent(event);
                assertEquals(victim.getPdfDocumentDescriptor().getFile(), event.getFile());
            }
        };
        eventStudio().add(OpenFileRequest.class, listener);
        typePathAndValidate();
        rightClick(".validable-container-field");
        click(DefaultI18nContext.getInstance().i18n("Open"));
        assertTrue(listener.isHit());
    }

    @Test
    public void openFolderMenuItem() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        HitTestListener<OpenFileRequest> listener = new HitTestListener<OpenFileRequest>() {
            @Override
            public void onEvent(OpenFileRequest event) {
                super.onEvent(event);
                assertEquals(victim.getPdfDocumentDescriptor().getFile().getParentFile(), event.getFile());
            }
        };
        eventStudio().add(OpenFileRequest.class, listener);
        typePathAndValidate();
        rightClick(".validable-container-field");
        click(DefaultI18nContext.getInstance().i18n("Open Folder"));
        assertTrue(listener.isHit());
    }

    @Test
    public void additionalOnLoadedConsumer() throws Exception {
        HitConsumer<PdfDocumentDescriptor> consumer = new HitConsumer<>();
        SingleSelectionPane victim = find("#victim-selection-pane");
        victim.addOnLoaded(consumer);
        moveToLoadedState(victim);
        assertTrue(consumer.isHit());
    }

    @Test
    public void onLoadedChangedSelectedPdfVersionEvent() throws Exception {
        Listener<ChangedSelectedPdfVersionEvent> listener = mock(Listener.class);
        eventStudio().add(ChangedSelectedPdfVersionEvent.class, listener, MODULE);
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedState(victim);
        verify(listener).onEvent(any());
    }

    @Test
    public void onDecryptedChangedSelectedPdfVersionEvent() throws Exception {
        Listener<ChangedSelectedPdfVersionEvent> listener = mock(Listener.class);
        eventStudio().add(ChangedSelectedPdfVersionEvent.class, listener, MODULE);
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedWithDecryption(victim);
        verify(listener).onEvent(any());
    }

    @Test
    public void onLoadedSetDestinationRequest() throws Exception {
        Listener<SetDestinationRequest> listener = mock(Listener.class);
        ArgumentCaptor<SetDestinationRequest> captor = ArgumentCaptor.forClass(SetDestinationRequest.class);
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        SingleSelectionPane victim = find("#victim-selection-pane");

        moveToLoadedState(victim);
        verify(listener).onEvent(captor.capture());
        assertTrue(captor.getValue().isFallback());
    }

    @Test
    public void onDecryptedSetDestinationRequest() throws Exception {
        Listener<SetDestinationRequest> listener = mock(Listener.class);
        ArgumentCaptor<SetDestinationRequest> captor = ArgumentCaptor.forClass(SetDestinationRequest.class);
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedWithDecryption(victim);
        verify(listener).onEvent(captor.capture());
        assertTrue(captor.getValue().isFallback());
    }

    @Test
    public void disableMenuOnInvalid() throws Exception {
        typePathAndValidate("/this/doesnt/exists");
        ValidableTextField victim = find(".validable-container-field");
        victim.getContextMenu().getItems().parallelStream().filter(i -> !(i instanceof SeparatorMenuItem))
                .forEach(i -> assertTrue(i.isDisable()));
    }

    @Test
    public void disableMenuOnSwitchToInvalid() throws Exception {
        typePathAndValidate();
        typePathAndValidate("/this/doesnt/exists");
        ValidableTextField victim = find(".validable-container-field");
        victim.getContextMenu().getItems().parallelStream().filter(i -> !(i instanceof SeparatorMenuItem))
                .forEach(i -> assertTrue(i.isDisable()));
    }

    @Test
    public void loadingStateDetails() throws Exception {
        typePathAndValidate();
        SingleSelectionPane victim = find("#victim-selection-pane");
        FXTestUtils.invokeAndWait(() -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
        }, 2);
        exists(DefaultI18nContext.getInstance().i18n("Loading..."));
    }

    @Test
    public void loadedDetails() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedState(victim);
        exists(DefaultI18nContext.getInstance().i18n("Pages: {0}, PDF Version: {1}", "0", ""));
    }

    @Test
    public void onSaveWorkspace() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedStateWithSpecialChars(victim);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertNull(data.get("victim-selection-paneinput.password"));
        assertThat(data.get("victim-selection-paneinput"), Matchers.endsWith("संसकरण_test.pdf"));
    }

    @Test
    public void onSaveWorkspaceWithPwd() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedWithDecryption(victim);
        victim.getPdfDocumentDescriptor().setPassword("pwd");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("pwd", data.get("victim-selection-paneinput.password"));
        assertThat(data.get("victim-selection-paneinput"), Matchers.endsWith("chuck.pdf"));
    }

    @Test
    public void onSaveWorkspaceEmptyDescriptor() {
        SingleSelectionPane victim = find("#victim-selection-pane");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(data.isEmpty());
    }

    @Test
    public void restoreStateFrom() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        @SuppressWarnings("rawtypes")
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        Map<String, String> data = new HashMap<>();
        data.put("victim-selection-paneinput", "chuck.pdf");
        data.put("victim-selection-paneinput.password", "pwd");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertEquals("chuck.pdf", victim.getField().getTextField().getText());
        assertEquals("pwd", victim.getPdfDocumentDescriptor().getPassword());
        verify(listener).onEvent(any());
    }

    @Test
    public void restoreStateFromEmpty() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedState(victim);
        Map<String, String> data = new HashMap<>();
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertTrue(isEmpty(victim.getField().getTextField().getText()));
    }

    @Test
    public void decryptedDetails() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedWithDecryption(victim);
        exists(DefaultI18nContext.getInstance().i18n("Pages: {0}, PDF Version: {1}", "0", ""));
    }

    @Test
    public void loadedDetailsSpecialChars() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedStateWithSpecialChars(victim);
        exists(DefaultI18nContext.getInstance().i18n("Pages: {0}, PDF Version: {1}", "0", ""));
    }

    @Test
    public void emptyDetailsOnSwithToInvalid() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedWithDecryption(victim);
        typePathAndValidate("/this/doesnt/exists");
        Label details = find(".-pdfsam-selection-details");
        assertTrue(isEmpty(details.getText()));
    }

    @Test
    public void clickWithErrorsShowsLogStage() throws Exception {
        typePathAndValidate();
        SingleSelectionPane victim = find("#victim-selection-pane");
        FXTestUtils.invokeAndWait(() -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.WITH_ERRORS);
        }, 2);
        Listener<ShowStageRequest> listener = mock(Listener.class);
        eventStudio().add(ShowStageRequest.class, listener, "LogStage");
        click(".glyph-icon");
        verify(listener).onEvent(any());
    }

    @Test
    public void clickEncryptedThrowsRequest() throws Exception {
        typePathAndValidate();
        SingleSelectionPane victim = find("#victim-selection-pane");
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        FXTestUtils.invokeAndWait(() -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        }, 2);

        click(".glyph-icon");
        type("pwd").click(DefaultI18nContext.getInstance().i18n("Unlock"));
        verify(listener, times(2)).onEvent(any());
    }

    @Test
    public void emptyStatusIndicatorOnSwithToInvalid() throws Exception {
        SingleSelectionPane victim = find("#victim-selection-pane");
        moveToLoadedWithDecryption(victim);
        typePathAndValidate("/this/doesnt/exists");
        Label encStatus = find(".encryption-status");
        assertTrue(isEmpty(encStatus.getText()));
    }

    @Test
    public void invalidatedDescriptorDoesntTriggerAnything() throws Exception {
        typePathAndValidate();
        typePathAndValidate("/this/doesnt/exists");
        SingleSelectionPane victim = find("#victim-selection-pane");
        FXTestUtils.invokeAndWait(() -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADED);
        }, 2);
        Label details = find(".-pdfsam-selection-details");
        assertTrue(isEmpty(details.getText()));
        Label encStatus = find(".encryption-status");
        assertTrue(isEmpty(encStatus.getText()));
        ValidableTextField field = find(".validable-container-field");
        field.getContextMenu().getItems().parallelStream().filter(i -> !(i instanceof SeparatorMenuItem))
                .forEach(i -> assertTrue(i.isDisable()));
    }

    private void moveToLoadedState(SingleSelectionPane victim) throws Exception {
        typePathAndValidate();
        FXTestUtils.invokeAndWait(() -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADED);
        }, 2);
    }

    private void moveToLoadedWithDecryption(SingleSelectionPane victim) throws Exception {
        typePathAndValidate();
        FXTestUtils.invokeAndWait(() -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION);
        }, 2);
    }

    private void moveToLoadedStateWithSpecialChars(SingleSelectionPane victim) throws Exception {
        typeSpecialPathAndValidate();
        FXTestUtils.invokeAndWait(() -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADED);
        }, 2);
    }

    private void typeSpecialPathAndValidate() throws Exception {
        File input = tmpFolder.newFile("संसकरण_test.pdf");
        typePathAndValidate(input.getAbsolutePath());
    }

    private void typePathAndValidate() throws Exception {
        File input = tmpFolder.newFile("chuck.pdf");
        typePathAndValidate(input.getAbsolutePath());
    }

    private void typePathAndValidate(String path) throws Exception {
        ValidableTextField field = find(".validable-container-field");
        // TODO replace with typing when slash works
        FXTestUtils.invokeAndWait(() -> field.setText(path), 2);
        click(field).type(KeyCode.TAB);
    }
}
