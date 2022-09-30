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
package org.pdfsam.ui.components.selection.single;

import javafx.scene.Scene;
import javafx.scene.control.Labeled;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.KeyCode;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.mockito.ArgumentCaptor;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.core.support.EncryptionUtils;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.io.NativeOpenFileRequest;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.ui.ChangedSelectedPdfVersionEvent;
import org.pdfsam.model.ui.SetDestinationRequest;
import org.pdfsam.model.ui.ShowPdfDescriptorRequest;
import org.pdfsam.model.ui.ShowStageRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitConsumer;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@SuppressWarnings("unchecked")
@ExtendWith(ApplicationExtension.class)
public class SingleSelectionPaneTest {
    private static final String MODULE = "MODULE";
    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(MODULE);
    @TempDir
    private Path folder;
    private SingleSelectionPane victim;
    private FxRobot robot;

    @BeforeAll
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleRequest(Locale.UK.toLanguageTag()));
        System.setProperty("testfx.robot.write_sleep", "5");
    }

    @Start
    public void start(Stage stage) {
        victim = new SingleSelectionPane(MODULE);
        victim.setId("victim-selection-pane");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void loadEvent() throws Exception {
        HitTestListener<PdfLoadRequest> listener = new HitTestListener<>();
        eventStudio().add(PdfLoadRequest.class, listener);
        typePathAndValidate();
        assertTrue(listener.isHit());
    }

    @Test
    @Tag("NoHeadless")
    public void infoMenuItem() throws Exception {
        HitTestListener<ShowPdfDescriptorRequest> listener = new HitTestListener<>();
        eventStudio().add(ShowPdfDescriptorRequest.class, listener);
        typePathAndValidate();
        robot.rightClickOn(".validable-container-field");
        robot.clickOn(i18n().tr("Document properties"));
        assertTrue(listener.isHit());
    }

    @Test
    @Tag("NoHeadless")
    public void setDestinationMenuItem() throws Exception {
        var listener = new HitTestListener<SetDestinationRequest>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertFalse(event.fallback());
            }
        };
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        typePathAndValidate();
        robot.rightClickOn(".validable-container-field");
        robot.clickOn(i18n().tr("Set destination"));
        assertTrue(listener.isHit());
    }

    @Test
    @Tag("NoHeadless")
    public void openFileMenuItem() throws Exception {
        var listener = new HitTestListener<NativeOpenFileRequest>() {
            @Override
            public void onEvent(NativeOpenFileRequest event) {
                super.onEvent(event);
                assertEquals(victim.getPdfDocumentDescriptor().getFile(), event.file());
            }
        };
        eventStudio().add(NativeOpenFileRequest.class, listener);
        typePathAndValidate();
        robot.rightClickOn(".validable-container-field");
        robot.clickOn(i18n().tr("Open"));
        assertTrue(listener.isHit());
    }

    @Test
    @Tag("NoHeadless")
    public void openFolderMenuItem() throws Exception {
        var listener = new HitTestListener<NativeOpenFileRequest>() {
            @Override
            public void onEvent(NativeOpenFileRequest event) {
                super.onEvent(event);
                assertEquals(victim.getPdfDocumentDescriptor().getFile().getParentFile(), event.file());
            }
        };
        eventStudio().add(NativeOpenFileRequest.class, listener);
        typePathAndValidate();
        robot.rightClickOn(".validable-container-field");
        robot.clickOn(i18n().tr("Open Folder"));
        assertTrue(listener.isHit());
    }

    @Test
    @Tag("NoHeadless")
    public void removeMenuItem() throws Exception {
        typePathAndValidate();
        robot.rightClickOn(".validable-container-field");
        robot.clickOn(i18n().tr("Remove"));
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
        assertTrue(captor.getValue().fallback());
    }

    @Test
    public void onDecryptedSetDestinationRequest() throws Exception {
        Listener<SetDestinationRequest> listener = mock(Listener.class);
        ArgumentCaptor<SetDestinationRequest> captor = ArgumentCaptor.forClass(SetDestinationRequest.class);
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        moveToLoadedWithDecryption(victim);
        verify(listener).onEvent(captor.capture());
        assertTrue(captor.getValue().fallback());
    }

    @Test
    public void disableMenuOnInvalid() {
        typePathAndValidate("/this/doesnt/exists");
        ValidableTextField victim = robot.lookup(".validable-container-field").queryAs(ValidableTextField.class);
        victim.getContextMenu().getItems().parallelStream().filter(i -> !(i instanceof SeparatorMenuItem))
                .filter(i -> !i.getText().equals(i18n().tr("Remove")))
                .forEach(i -> assertTrue(i.isDisable(), i.getText()));
    }

    @Test
    public void disableMenuOnSwitchToInvalid() throws Exception {
        typePathAndValidate();
        typePathAndValidate("/this/doesnt/exists");
        ValidableTextField victim = robot.lookup(".validable-container-field").queryAs(ValidableTextField.class);
        victim.getContextMenu().getItems().parallelStream().filter(i -> !(i instanceof SeparatorMenuItem))
                .filter(i -> !i.getText().equals(i18n().tr("Remove"))).forEach(i -> assertTrue(i.isDisable()));
    }

    @Test
    public void loadingStateDetails() throws Exception {
        typePathAndValidate();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
        });
        assertTrue(robot.lookup(i18n().tr("Loading...")).tryQuery().isPresent());
    }

    @Test
    public void loadedDetails() throws Exception {
        moveToLoadedState(victim);
        assertTrue(robot.lookup(i18n().tr("Pages: {0}, PDF Version: {1}", "0", "")).tryQuery().isPresent());
    }

    @Test
    public void onSaveWorkspace() throws Exception {
        moveToLoadedStateWithSpecialChars(victim);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertNull(data.get("victim-selection-paneinput.password"));
        assertThat(data.get("victim-selection-paneinput")).endsWith("संसकरण_test.pdf");
    }

    @Test
    public void onSaveWorkspaceWithPwd() throws Exception {
        app().persistentSettings().set(BooleanPersistentProperty.SAVE_PWD_IN_WORKSPACE, true);
        moveToLoadedWithDecryption(victim);
        victim.getPdfDocumentDescriptor().setPassword("pwd");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals(EncryptionUtils.encrypt("pwd"), data.get("victim-selection-paneinput.password.enc"));
        assertThat(data.get("victim-selection-paneinput")).endsWith("chuck.pdf");
    }

    @Test
    public void onSaveWorkspaceWithoutPwd() throws Exception {
        app().persistentSettings().set(BooleanPersistentProperty.SAVE_PWD_IN_WORKSPACE, false);
        moveToLoadedWithDecryption(victim);
        victim.getPdfDocumentDescriptor().setPassword("pwd");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(isBlank(data.get("victim-selection-paneinput.password")));
        assertTrue(isBlank(data.get("victim-selection-paneinput.password.enc")));
        assertThat(data.get("victim-selection-paneinput")).endsWith("chuck.pdf");
    }

    @Test
    public void onSaveWorkspaceEmptyDescriptor() {
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(data.isEmpty());
    }

    @Test
    public void restoreStateFromPwdBackwardCompatible() {
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener);
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
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener);
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
        assertTrue(robot.lookup(i18n().tr("Pages: {0}, PDF Version: {1}", "0", "")).tryQuery().isPresent());
    }

    @Test
    public void loadedDetailsSpecialChars() throws Exception {
        moveToLoadedStateWithSpecialChars(victim);
        assertTrue(robot.lookup(i18n().tr("Pages: {0}, PDF Version: {1}", "0", "")).tryQuery().isPresent());
    }

    @Test
    public void emptyDetailsOnSwithToInvalid() throws Exception {
        moveToLoadedWithDecryption(victim);
        typePathAndValidate("/this/doesnt/exists");
        Labeled details = robot.lookup(".-pdfsam-selection-details").queryLabeled();
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
        robot.clickOn(String.valueOf((char) UniconsLine.EXCLAMATION_CIRCLE.getCode()));
        verify(listener).onEvent(any());
    }

    @Test
    @Tag("NoHeadless")
    public void clickEncryptedThrowsRequest() throws Exception {
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener);
        moveToEncrytedState(victim);
        robot.clickOn(String.valueOf((char) UniconsLine.LOCK.getCode()));
        robot.write("pwd").clickOn(i18n().tr("Unlock"));
        verify(listener, times(2)).onEvent(any());
    }

    @Test
    public void emptyStatusIndicatorOnSwithToInvalid() throws Exception {
        moveToLoadedWithDecryption(victim);
        typePathAndValidate("/this/doesnt/exists");
        Labeled encStatus = robot.lookup(".encryption-status").queryLabeled();
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
        Labeled details = robot.lookup(".-pdfsam-selection-details").queryLabeled();
        assertTrue(isEmpty(details.getText()));
        Labeled encStatus = robot.lookup(".encryption-status").queryLabeled();
        assertTrue(isEmpty(encStatus.getText()));
        var field = robot.lookup(".validable-container-field").queryAs(ValidableTextField.class);
        field.getContextMenu().getItems().parallelStream().filter(i -> !(i instanceof SeparatorMenuItem))
                .filter(i -> !i.getText().equals(i18n().tr("Remove"))).forEach(i -> assertTrue(i.isDisable()));
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
        File input = Files.createFile(folder.resolve("संसकरण_test.pdf")).toFile();
        typePathAndValidate(input.getAbsolutePath());
    }

    private void typePathAndValidate() throws Exception {
        File input = Files.createFile(folder.resolve("chuck.pdf")).toFile();
        typePathAndValidate(input.getAbsolutePath());
    }

    private void typePathAndValidate(String path) {
        robot.clickOn(".validable-container-field").write(path).type(KeyCode.TAB);
    }
}
