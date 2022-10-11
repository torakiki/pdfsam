/*
 * This file is part of the PDF Split And Merge source code
 * Created on 11/ago/2014
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
package org.pdfsam.ui.components.selection.multiple;

import javafx.collections.FXCollections;
import javafx.scene.Scene;
import javafx.scene.input.Clipboard;
import javafx.scene.input.DataFormat;
import javafx.scene.input.KeyCode;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.core.support.EncryptionUtils;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.io.NativeOpenFileRequest;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.model.ui.SetDestinationRequest;
import org.pdfsam.model.ui.ShowPdfDescriptorRequest;
import org.pdfsam.model.ui.ShowStageRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.components.selection.RemoveSelectedEvent;
import org.pdfsam.ui.components.selection.multiple.move.MoveSelectedEvent;
import org.pdfsam.ui.components.selection.multiple.move.MoveType;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.testfx.api.FxAssert.verifyThat;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
public class SelectionTableTest {

    private static final String MODULE = "MODULE";
    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(MODULE);
    @TempDir
    public Path folder;
    private FxRobot robot;
    private SelectionTable victim;
    private PdfDocumentDescriptor firstItem;

    @Start
    public void start(Stage stage) throws Exception {
        victim = new SelectionTable(MODULE, true, true,
                new SelectionTableColumn<?>[] { new LoadingColumn(MODULE), FileColumn.NAME, LongColumn.SIZE,
                        IntColumn.PAGES, LongColumn.LAST_MODIFIED, new PageRangesColumn() });
        victim.setId("victim");
        firstItem = populate();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @AfterEach
    public void tearDown() {
        robot.type(KeyCode.ESCAPE);
    }

    @Test
    public void fallbackRequest() {
        HitTestListener<SetDestinationRequest> listener = new HitTestListener<>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertTrue(event.fallback());
            }
        };
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        assertTrue(listener.isHit());
    }

    @Test
    public void select() {
        HitTestListener<SelectionChangedEvent> listener = new HitTestListener<>() {
            @Override
            public void onEvent(SelectionChangedEvent event) {
                super.onEvent(event);
                assertTrue(event.isSingleSelection());
            }
        };
        eventStudio().add(SelectionChangedEvent.class, listener, MODULE);
        robot.clickOn("temp.pdf");
        assertTrue(listener.isHit());
    }

    @Test
    public void multipleSelect() {
        HitTestListener<SelectionChangedEvent> listener = new HitTestListener<>() {
            @Override
            public void onEvent(SelectionChangedEvent event) {
                super.onEvent(event);
                assertFalse(event.isSingleSelection());
            }
        };
        robot.clickOn("temp.pdf").press(KeyCode.CONTROL);
        eventStudio().add(SelectionChangedEvent.class, listener, MODULE);
        robot.clickOn("temp3.pdf");
        robot.release(KeyCode.CONTROL);
        assertTrue(listener.isHit());
    }

    @Test
    public void itemsAdded() {
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener);
        assertEquals(4, victim.getItems().size());
        verify(listener).onEvent(any());
    }

    @Test
    public void onSaveWorkspace() {
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("4", data.get("victiminput.size"));
        assertThat(data.get("victiminput.0")).endsWith("temp.pdf");
        assertThat(data.get("victiminput.1")).endsWith("®¯°±²³要选择需要转换的文.pdf");
        assertThat(data.get("victiminput.2")).endsWith("temp3.pdf");
        assertThat(data.get("victiminput.3")).endsWith("temp4.pdf");
    }

    @Test
    public void onSaveWorkspaceEmpty() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.setItems(FXCollections.observableArrayList()));
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("0", data.get("victiminput.size"));
        assertNull(data.get("victiminput.0"));
    }

    @Test
    @Tag("NoHeadless")
    public void onSaveWorkspaceEncryptedPwdStored() {
        app().persistentSettings().set(BooleanPersistentProperty.SAVE_PWD_IN_WORKSPACE, true);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        });
        WaitForAsyncUtils.waitForFxEvents();
        robot.clickOn(".ikonli-font-icon");
        robot.write("pwd").clickOn(i18n().tr("Unlock"));
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals(EncryptionUtils.encrypt("pwd"), data.get("victiminput.password.enc0"));
    }

    @Test
    public void onSaveWorkspaceEncryptedNoPwdStored() {
        app().persistentSettings().set(BooleanPersistentProperty.SAVE_PWD_IN_WORKSPACE, false);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        });
        WaitForAsyncUtils.waitForFxEvents();
        robot.clickOn(".ikonli-font-icon");
        robot.write("pwd").clickOn(i18n().tr("Unlock"));
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(isBlank(data.get("victiminput.password.enc0")));
        assertTrue(isBlank(data.get("victiminput.password.0")));
    }

    @Test
    public void restoreStateFromPwdBackwardCompatible() {
        eventStudio().clear();
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener);
        Map<String, String> data = new HashMap<>();
        data.put("victiminput.size", "1");
        data.put("victiminput.0", "chuck.pdf");
        data.put("victiminput.password.0", "pwd");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(1, victim.getItems().size());
        SelectionTableRowData first = victim.getItems().get(0);
        assertEquals("pwd", first.descriptor().getPassword());
    }

    @Test
    public void restoreStateFrom() {
        eventStudio().clear();
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener);
        Map<String, String> data = new HashMap<>();
        data.put("victiminput.size", "2");
        data.put("victiminput.0", "chuck.pdf");
        data.put("victiminput.password.enc0", EncryptionUtils.encrypt("pwd"));
        data.put("victiminput.range.0", "1-10");
        data.put("victiminput.step.0", "4");
        data.put("victiminput.reverse.0", "true");
        data.put("victiminput.1", "norris.pdf");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(2, victim.getItems().size());
        SelectionTableRowData first = victim.getItems().get(0);
        assertEquals("chuck.pdf", first.descriptor().getFileName());
        assertEquals("pwd", first.descriptor().getPassword());
        assertEquals("1-10", first.pageSelection.get());
        assertEquals("4", first.pace.get());
        assertTrue(first.reverse.get());
        SelectionTableRowData second = victim.getItems().get(1);
        assertEquals("norris.pdf", second.descriptor().getFileName());
        assertNull(second.pageSelection.get());
        assertNull(second.pace.get());
        assertFalse(second.reverse.get());
        verify(listener).onEvent(any());
    }

    @Test
    public void restoreStateFromEmpty() {
        Map<String, String> data = new HashMap<>();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertTrue(victim.getItems().isEmpty());
    }

    @Test
    public void restoreStateFromSizeZero() {
        Map<String, String> data = new HashMap<>();
        data.put("victiminput.size", "0");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertTrue(victim.getItems().isEmpty());
    }

    @Test
    public void indexColumn() {
        org.testfx.assertions.api.Assertions.assertThat(victim).hasTableCell("1");
        org.testfx.assertions.api.Assertions.assertThat(victim).hasTableCell("2");
        org.testfx.assertions.api.Assertions.assertThat(victim).hasTableCell("3");
        org.testfx.assertions.api.Assertions.assertThat(victim).hasTableCell("4");
        org.testfx.assertions.api.Assertions.assertThat(victim).doesNotHaveTableCell("5");
    }

    @Test
    public void clear() {
        robot.clickOn("temp.pdf");
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
        WaitForAsyncUtils.waitForAsyncFx(2000,
                () -> eventStudio().broadcast(new ClearToolRequest(MODULE, false, false), MODULE));
        assertTrue(victim.getSelectionModel().getSelectedIndices().isEmpty());
    }

    @Test
    @Tag("NoHeadless")
    public void encryptedThrowsRequest() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        });
        WaitForAsyncUtils.waitForFxEvents();
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener);
        robot.clickOn(".ikonli-font-icon");
        robot.write("pwd").clickOn(i18n().tr("Unlock"));
        verify(listener, times(2)).onEvent(any());
    }

    @Test
    public void clearSelectionByclickOn() {
        robot.clickOn("temp.pdf");
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
        Listener<SelectionChangedEvent> listener = mock(Listener.class);
        ArgumentCaptor<SelectionChangedEvent> captor = ArgumentCaptor.forClass(SelectionChangedEvent.class);
        eventStudio().add(SelectionChangedEvent.class, listener, MODULE);
        robot.press(KeyCode.CONTROL).clickOn("temp.pdf");
        assertTrue(victim.getSelectionModel().getSelectedIndices().isEmpty());
        robot.release(KeyCode.CONTROL);
        verify(listener).onEvent(captor.capture());
        assertTrue(captor.getValue().isClearSelection());
    }

    @Test
    @Tag("NoHeadless")
    public void removeByContextMenu() {
        robot.rightClickOn("temp.pdf");
        robot.clickOn(i18n().tr("Remove"));
        assertEquals(3, victim.getItems().size());
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
    }

    @Test
    public void removeMultiple() {
        robot.clickOn("temp.pdf").press(KeyCode.CONTROL).clickOn("temp3.pdf").release(KeyCode.CONTROL);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> eventStudio().broadcast(new RemoveSelectedEvent(), MODULE));
        assertEquals(2, victim.getItems().size());
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
    }

    @Test
    public void removeRelease() {
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        assertTrue(item.isPresent());
        robot.clickOn("temp.pdf");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> eventStudio().broadcast(new RemoveSelectedEvent(), MODULE));
        assertFalse(item.get().descriptor().hasReferences());
    }

    @Test
    @Tag("NoHeadless")
    public void clearInvalidatesDuplicatedItems() {
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        robot.rightClickOn("temp.pdf");
        robot.clickOn(i18n().tr("Duplicate"));
        WaitForAsyncUtils.waitForAsyncFx(2000,
                () -> eventStudio().broadcast(new ClearToolRequest(MODULE, false, false), MODULE));
        assertFalse(item.get().descriptor().hasReferences());
    }

    @Test
    @Tag("NoHeadless")
    public void duplicate() {
        robot.rightClickOn("temp.pdf");
        robot.clickOn(i18n().tr("Duplicate"));
        assertEquals(2,
                victim.getItems().stream().filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).count());
    }

    @Test
    public void moveSelected() {
        robot.clickOn("temp.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 0);
        WaitForAsyncUtils.waitForAsyncFx(2000,
                () -> eventStudio().broadcast(new MoveSelectedEvent(MoveType.DOWN), MODULE));
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 1);
    }

    @Test
    @Tag("NoHeadless")
    public void moveDownByContextMenu() {
        robot.rightClickOn("temp.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 0);
        robot.clickOn(i18n().tr("Move Down"));
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 1);
    }

    @Test
    @Tag("NoHeadless")
    public void moveBottomByContextMenu() {
        robot.rightClickOn("temp.pdf");
        assertEquals(0, victim.getSelectionModel().getSelectedIndex());
        robot.clickOn(i18n().tr("Move to Bottom"));
        assertEquals(3, victim.getSelectionModel().getSelectedIndex());
    }

    @Test
    @Tag("NoHeadless")
    public void moveUpByContextMenu() {
        robot.rightClickOn("temp3.pdf");
        assertEquals(2, victim.getSelectionModel().getSelectedIndex());
        robot.clickOn(i18n().tr("Move Up"));
        assertEquals(1, victim.getSelectionModel().getSelectedIndex());
    }

    @Test
    @Tag("NoHeadless")
    public void moveTopByContextMenu() {
        robot.rightClickOn("temp3.pdf");
        assertEquals(2, victim.getSelectionModel().getSelectedIndex());
        robot.clickOn(i18n().tr("Move to Top"));
        assertEquals(0, victim.getSelectionModel().getSelectedIndex());
    }

    @Test
    @Tag("NoHeadless")
    public void copy() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> Clipboard.getSystemClipboard().clear());
        robot.rightClickOn("temp.pdf");
        robot.clickOn(i18n().tr("Copy to clipboard"));
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> assertFalse(
                StringUtils.isEmpty(Clipboard.getSystemClipboard().getContent(DataFormat.PLAIN_TEXT).toString())));
    }

    @Test
    @Tag("NoHeadless")
    public void pageRangesForAllByContextMenu() {
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        assertTrue(item.isPresent());
        item.get().pageSelection.set("2-4");
        robot.rightClickOn("temp.pdf");
        robot.clickOn(i18n().tr("Set as range for all"));
        victim.getItems().forEach(i -> assertEquals("2-4", i.pageSelection.get()));
    }

    @Test
    @Tag("NoHeadless")
    public void setDestinationByContextMenu() {
        HitTestListener<SetDestinationRequest> listener = new HitTestListener<>();
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        robot.rightClickOn("temp3.pdf");
        HitTestListener<SetDestinationRequest> notFallbackListener = new HitTestListener<>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertFalse(event.fallback());
            }
        };
        eventStudio().add(SetDestinationRequest.class, notFallbackListener, MODULE);
        robot.clickOn(i18n().tr("Set destination"));
        assertTrue(listener.isHit());
        assertTrue(notFallbackListener.isHit());
    }

    @Test
    @Tag("NoHeadless")
    public void openByContextMenu() {
        HitTestListener<NativeOpenFileRequest> listener = new HitTestListener<>();
        eventStudio().add(NativeOpenFileRequest.class, listener);
        robot.rightClickOn("temp3.pdf");
        robot.clickOn(i18n().tr("Open"));
        assertTrue(listener.isHit());
    }

    @Test
    @Tag("NoHeadless")
    public void openFolderByContextMenu() {
        HitTestListener<NativeOpenFileRequest> listener = new HitTestListener<>();
        eventStudio().add(NativeOpenFileRequest.class, listener);
        robot.rightClickOn("temp3.pdf");
        robot.clickOn(i18n().tr("Open Folder"));
        assertTrue(listener.isHit());
    }

    @Test
    @Tag("NoHeadless")
    public void infoByContextMenu() {
        Listener<ShowPdfDescriptorRequest> listener = mock(Listener.class);
        eventStudio().add(ShowPdfDescriptorRequest.class, listener);
        robot.rightClickOn("temp3.pdf");
        robot.clickOn(i18n().tr("Document properties"));
        verify(listener, timeout(2000)).onEvent(any());
    }

    @Test
    @Tag("NoWindows")
    public void iconsAreShown() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED));
        WaitForAsyncUtils.waitForFxEvents();
        Text icon = robot.lookup(".ikonli-font-icon").queryAs(Text.class);
        assertEquals(String.valueOf((char) PdfDescriptorLoadingStatus.REQUESTED.getIcon().getCode()), icon.getText());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING));
        WaitForAsyncUtils.waitForFxEvents();
        icon = robot.lookup(".ikonli-font-icon").queryAs(Text.class);
        assertEquals(String.valueOf((char) PdfDescriptorLoadingStatus.LOADING.getIcon().getCode()), icon.getText());
    }

    @Test
    public void clickWithErrorsShowsLogStage() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.WITH_ERRORS);
        });
        WaitForAsyncUtils.waitForFxEvents();
        Listener<ShowStageRequest> listener = mock(Listener.class);
        eventStudio().add(ShowStageRequest.class, listener, "LogStage");
        robot.clickOn(".ikonli-font-icon");
        verify(listener).onEvent(any());
    }

    @Test
    public void logEventOnclickOn() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED));
        WaitForAsyncUtils.waitForFxEvents();
        Text icon = robot.lookup(".ikonli-font-icon").queryAs(Text.class);
        assertEquals(String.valueOf((char) PdfDescriptorLoadingStatus.REQUESTED.getIcon().getCode()), icon.getText());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING));
        WaitForAsyncUtils.waitForFxEvents();
        icon = robot.lookup(".ikonli-font-icon").queryAs(Text.class);
        assertEquals(String.valueOf((char) PdfDescriptorLoadingStatus.LOADING.getIcon().getCode()), icon.getText());
    }

    @Test
    @Tag("NoHeadless")
    @Disabled("TODO")
    public void editCommitOnFocusLost() {
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        assertTrue(item.isPresent());
        item.get().pageSelection.set("2");
        WaitForAsyncUtils.waitForFxEvents();
        robot.clickOn("2").type(KeyCode.ENTER, KeyCode.DIGIT5);
        robot.clickOn("temp4.pdf");
        WaitForAsyncUtils.waitForFxEvents();
        assertEquals(item.get().pageSelection.get(), "5");
    }

    private PdfDocumentDescriptor populate() throws Exception {
        File file = Files.createFile(folder.resolve("temp.pdf")).toFile();
        File file2 = Files.createFile(folder.resolve("®¯°±²³要选择需要转换的文.pdf")).toFile();
        File file3 = Files.createFile(folder.resolve("temp3.pdf")).toFile();
        File file4 = Files.createFile(folder.resolve("temp4.pdf")).toFile();
        PdfLoadRequest loadEvent = new PdfLoadRequest(MODULE);
        PdfDocumentDescriptor ret = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        loadEvent.add(ret);
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file2));
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file3));
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file4));
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> eventStudio().broadcast(loadEvent, MODULE));
        return ret;
    }
}
