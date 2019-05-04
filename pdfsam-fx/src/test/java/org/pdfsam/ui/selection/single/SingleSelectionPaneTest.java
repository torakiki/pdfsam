/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/lug/2014
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
package org.pdfsam.ui.selection.single;

import static org.apache.commons.lang3.StringUtils.isBlank;
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
import org.mockito.ArgumentCaptor;
import org.pdfsam.NoHeadless;
import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.EncryptionUtils;
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
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import javafx.scene.Scene;
import javafx.scene.control.Labeled;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.KeyCode;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
@SuppressWarnings("unchecked")
public class SingleSelectionPaneTest extends ApplicationTest {

    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder tmpFolder = new TemporaryFolder();
    private SingleSelectionPane victim;

    @BeforeClass
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
        System.setProperty("testfx.robot.write_sleep", "5");
    }

    @Override
    public void start(Stage stage) {
        victim = new SingleSelectionPane(MODULE);
        victim.setId("victim-selection-pane");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void loadEvent() throws Exception {
        HitTestListener<PdfLoadRequestEvent> listener = new HitTestListener<>();
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        typePathAndValidate();
        assertTrue(listener.isHit());
    }

    @Test
    @Category(NoHeadless.class)
    public void infoMenuItem() throws Exception {
        HitTestListener<ShowPdfDescriptorRequest> listener = new HitTestListener<>();
        eventStudio().add(ShowPdfDescriptorRequest.class, listener);
        typePathAndValidate();
        rightClickOn(".validable-container-field");
        clickOn(DefaultI18nContext.getInstance().i18n("Document properties"));
        assertTrue(listener.isHit());
    }

    @Test
    @Category(NoHeadless.class)
    public void setDestinationMenuItem() throws Exception {
        var listener = new HitTestListener<SetDestinationRequest>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertFalse(event.isFallback());
            }
        };
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        typePathAndValidate();
        rightClickOn(".validable-container-field");
        clickOn(DefaultI18nContext.getInstance().i18n("Set destination"));
        assertTrue(listener.isHit());
    }

    @Test
    @Category(NoHeadless.class)
    public void openFileMenuItem() throws Exception {
        var listener = new HitTestListener<OpenFileRequest>() {
            @Override
            public void onEvent(OpenFileRequest event) {
                super.onEvent(event);
                assertEquals(victim.getPdfDocumentDescriptor().getFile(), event.getFile());
            }
        };
        eventStudio().add(OpenFileRequest.class, listener);
        typePathAndValidate();
        rightClickOn(".validable-container-field");
        clickOn(DefaultI18nContext.getInstance().i18n("Open"));
        assertTrue(listener.isHit());
    }

    @Test
    @Category(NoHeadless.class)
    public void openFolderMenuItem() throws Exception {
        var listener = new HitTestListener<OpenFileRequest>() {
            @Override
            public void onEvent(OpenFileRequest event) {
                super.onEvent(event);
                assertEquals(victim.getPdfDocumentDescriptor().getFile().getParentFile(), event.getFile());
            }
        };
        eventStudio().add(OpenFileRequest.class, listener);
        typePathAndValidate();
        rightClickOn(".validable-container-field");
        clickOn(DefaultI18nContext.getInstance().i18n("Open Folder"));
        assertTrue(listener.isHit());
    }

    @Test
    @Category(NoHeadless.class)
    public void removeMenuItem() throws Exception {
        typePathAndValidate();
        rightClickOn(".validable-container-field");
        clickOn(DefaultI18nContext.getInstance().i18n("Remove"));
        assertTrue(isEmpty(victim.getField().getTextField().getText()));
    }

    @Test
    public void additionalOnLoadedConsumer() throws Exception {
        HitConsumer<PdfDocumentDescriptor> consumer = new HitConsumer<>();
        victim.addOnLoaded(consumer);
        moveToLoadedState(victim);
        assertTrue(consumer.isHit());
    }

    @Test
    public void additionalOnLoadedConsumerNotHit() throws Exception {
        HitConsumer<PdfDocumentDescriptor> consumer = new HitConsumer<>();
        victim.addOnLoaded(consumer);
        moveToEncrytedState(victim);
        assertFalse(consumer.isHit());
    }

    @Test
    public void onLoadedChangedSelectedPdfVersionEvent() throws Exception {
        Listener<ChangedSelectedPdfVersionEvent> listener = mock(Listener.class);
        eventStudio().add(ChangedSelectedPdfVersionEvent.class, listener, MODULE);
        moveToLoadedState(victim);
        verify(listener).onEvent(any());
    }

    @Test
    public void onDecryptedChangedSelectedPdfVersionEvent() throws Exception {
        Listener<ChangedSelectedPdfVersionEvent> listener = mock(Listener.class);
        eventStudio().add(ChangedSelectedPdfVersionEvent.class, listener, MODULE);
        moveToLoadedWithDecryption(victim);
        verify(listener).onEvent(any());
    }

    @Test
    public void onLoadedSetDestinationRequest() throws Exception {
        Listener<SetDestinationRequest> listener = mock(Listener.class);
        ArgumentCaptor<SetDestinationRequest> captor = ArgumentCaptor.forClass(SetDestinationRequest.class);
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);

        moveToLoadedState(victim);
        verify(listener).onEvent(captor.capture());
        assertTrue(captor.getValue().isFallback());
    }

    @Test
    public void onDecryptedSetDestinationRequest() throws Exception {
        Listener<SetDestinationRequest> listener = mock(Listener.class);
        ArgumentCaptor<SetDestinationRequest> captor = ArgumentCaptor.forClass(SetDestinationRequest.class);
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        moveToLoadedWithDecryption(victim);
        verify(listener).onEvent(captor.capture());
        assertTrue(captor.getValue().isFallback());
    }

    @Test
    public void disableMenuOnInvalid() {
        typePathAndValidate("/this/doesnt/exists");
        ValidableTextField victim = lookup(".validable-container-field").queryAs(ValidableTextField.class);
        victim.getContextMenu().getItems().parallelStream().filter(i -> !(i instanceof SeparatorMenuItem))
                .filter(i -> !i.getText().equals(DefaultI18nContext.getInstance().i18n("Remove")))
                .forEach(i -> assertTrue(i.getText(), i.isDisable()));
    }

    @Test
    public void disableMenuOnSwitchToInvalid() throws Exception {
        typePathAndValidate();
        typePathAndValidate("/this/doesnt/exists");
        ValidableTextField victim = lookup(".validable-container-field").queryAs(ValidableTextField.class);
        victim.getContextMenu().getItems().parallelStream().filter(i -> !(i instanceof SeparatorMenuItem))
                .filter(i -> !i.getText().equals(DefaultI18nContext.getInstance().i18n("Remove")))
                .forEach(i -> assertTrue(i.isDisable()));
    }

    @Test
    public void loadingStateDetails() throws Exception {
        typePathAndValidate();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
        });
        assertTrue(lookup(DefaultI18nContext.getInstance().i18n("Loading...")).tryQuery().isPresent());
    }

    @Test
    public void loadedDetails() throws Exception {
        moveToLoadedState(victim);
        assertTrue(lookup(DefaultI18nContext.getInstance().i18n("Pages: {0}, PDF Version: {1}", "0", "")).tryQuery()
                .isPresent());
    }

    @Test
    public void onSaveWorkspace() throws Exception {
        moveToLoadedStateWithSpecialChars(victim);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertNull(data.get("victim-selection-paneinput.password"));
        assertThat(data.get("victim-selection-paneinput"), Matchers.endsWith("संसकरण_test.pdf"));
    }

    @Test
    public void onSaveWorkspaceWithPwd() throws Exception {
        new DefaultUserContext().setBooleanPreference(BooleanUserPreference.SAVE_PWD_IN_WORKSPACE, true);
        moveToLoadedWithDecryption(victim);
        victim.getPdfDocumentDescriptor().setPassword("pwd");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals(EncryptionUtils.encrypt("pwd"), data.get("victim-selection-paneinput.password.enc"));
        assertThat(data.get("victim-selection-paneinput"), Matchers.endsWith("chuck.pdf"));
    }

    @Test
    public void onSaveWorkspaceWithoutPwd() throws Exception {
        new DefaultUserContext().setBooleanPreference(BooleanUserPreference.SAVE_PWD_IN_WORKSPACE, false);
        moveToLoadedWithDecryption(victim);
        victim.getPdfDocumentDescriptor().setPassword("pwd");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(isBlank(data.get("victim-selection-paneinput.password")));
        assertTrue(isBlank(data.get("victim-selection-paneinput.password.enc")));
        assertThat(data.get("victim-selection-paneinput"), Matchers.endsWith("chuck.pdf"));
    }

    @Test
    public void onSaveWorkspaceEmptyDescriptor() {
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(data.isEmpty());
    }

    @Test
    public void restoreStateFromPwdBackwardCompatible() {
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        Map<String, String> data = new HashMap<>();
        data.put("victim-selection-paneinput", "chuck.pdf");
        data.put("victim-selection-paneinput.password", "pwd");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals("chuck.pdf", victim.getField().getTextField().getText());
        assertEquals("pwd", victim.getPdfDocumentDescriptor().getPassword());
        verify(listener).onEvent(any());
    }

    @Test
    public void restoreStateFrom() {
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        Map<String, String> data = new HashMap<>();
        data.put("victim-selection-paneinput", "chuck.pdf");
        data.put("victim-selection-paneinput.password.enc", EncryptionUtils.encrypt("pwd"));
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals("chuck.pdf", victim.getField().getTextField().getText());
        assertEquals("pwd", victim.getPdfDocumentDescriptor().getPassword());
        verify(listener).onEvent(any());
    }

    @Test
    public void restoreStateFromEmpty() throws Exception {
        moveToLoadedState(victim);
        Map<String, String> data = new HashMap<>();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertTrue(isEmpty(victim.getField().getTextField().getText()));
    }

    @Test
    public void decryptedDetails() throws Exception {
        moveToLoadedWithDecryption(victim);
        assertTrue(lookup(DefaultI18nContext.getInstance().i18n("Pages: {0}, PDF Version: {1}", "0", "")).tryQuery()
                .isPresent());
    }

    @Test
    public void loadedDetailsSpecialChars() throws Exception {
        moveToLoadedStateWithSpecialChars(victim);
        assertTrue(lookup(DefaultI18nContext.getInstance().i18n("Pages: {0}, PDF Version: {1}", "0", "")).tryQuery()
                .isPresent());
    }

    @Test
    public void emptyDetailsOnSwithToInvalid() throws Exception {
        moveToLoadedWithDecryption(victim);
        typePathAndValidate("/this/doesnt/exists");
        Labeled details = lookup(".-pdfsam-selection-details").queryLabeled();
        assertTrue(isEmpty(details.getText()));
    }

    @Test
    public void clickWithErrorsShowsLogStage() throws Exception {
        typePathAndValidate();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.WITH_ERRORS);
        });
        Listener<ShowStageRequest> listener = mock(Listener.class);
        eventStudio().add(ShowStageRequest.class, listener, "LogStage");
        clickOn(FontAwesomeIcon.WARNING.unicode());
        verify(listener).onEvent(any());
    }

    @Test
    @Category(NoHeadless.class)
    public void clickEncryptedThrowsRequest() throws Exception {
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        moveToEncrytedState(victim);
        clickOn(FontAwesomeIcon.LOCK.unicode());
        write("pwd").clickOn(DefaultI18nContext.getInstance().i18n("Unlock"));
        verify(listener, times(2)).onEvent(any());
    }

    @Test
    public void emptyStatusIndicatorOnSwithToInvalid() throws Exception {
        moveToLoadedWithDecryption(victim);
        typePathAndValidate("/this/doesnt/exists");
        Labeled encStatus = lookup(".encryption-status").queryLabeled();
        assertTrue(isEmpty(encStatus.getText()));
    }

    @Test
    public void invalidatedDescriptorDoesntTriggerAnything() throws Exception {
        typePathAndValidate();
        typePathAndValidate("/this/doesnt/exists");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADED);
        });
        Labeled details = lookup(".-pdfsam-selection-details").queryLabeled();
        assertTrue(isEmpty(details.getText()));
        Labeled encStatus = lookup(".encryption-status").queryLabeled();
        assertTrue(isEmpty(encStatus.getText()));
        var field = lookup(".validable-container-field").queryAs(ValidableTextField.class);
        field.getContextMenu().getItems().parallelStream().filter(i -> !(i instanceof SeparatorMenuItem))
                .filter(i -> !i.getText().equals(DefaultI18nContext.getInstance().i18n("Remove")))
                .forEach(i -> assertTrue(i.isDisable()));
    }

    private void moveToLoadedState(SingleSelectionPane victim) throws Exception {
        typePathAndValidate();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADED);
        });
    }

    private void moveToEncrytedState(SingleSelectionPane victim) throws Exception {
        typePathAndValidate();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        });
    }

    private void moveToLoadedWithDecryption(SingleSelectionPane victim) throws Exception {
        typePathAndValidate();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION);
        });
    }

    private void moveToLoadedStateWithSpecialChars(SingleSelectionPane victim) throws Exception {
        typeSpecialPathAndValidate();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADED);
        });
    }

    private void typeSpecialPathAndValidate() throws Exception {
        File input = tmpFolder.newFile("संसकरण_test.pdf");
        typePathAndValidate(input.getAbsolutePath());
    }

    private void typePathAndValidate() throws Exception {
        File input = tmpFolder.newFile("chuck.pdf");
        typePathAndValidate(input.getAbsolutePath());
    }

    private void typePathAndValidate(String path) {
        clickOn(".validable-container-field").write(path).type(KeyCode.TAB);
    }
}
