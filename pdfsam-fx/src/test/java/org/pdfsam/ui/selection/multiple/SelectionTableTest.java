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
package org.pdfsam.ui.selection.multiple;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.testfx.api.FxAssert.verifyThat;
import static org.testfx.matcher.control.TableViewMatchers.hasTableCell;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.hamcrest.Matchers;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.pdfsam.NoHeadless;
import org.pdfsam.NoWindows;
import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.EncryptionUtils;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.commons.OpenFileRequest;
import org.pdfsam.ui.commons.RemoveSelectedEvent;
import org.pdfsam.ui.commons.SetDestinationRequest;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.pdfsam.ui.selection.multiple.move.MoveSelectedEvent;
import org.pdfsam.ui.selection.multiple.move.MoveType;
import org.pdfsam.eventstudio.Listener;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.collections.FXCollections;
import javafx.scene.Scene;
import javafx.scene.input.Clipboard;
import javafx.scene.input.DataFormat;
import javafx.scene.input.KeyCode;
import javafx.scene.text.Text;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class SelectionTableTest extends ApplicationTest {

    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private SelectionTable victim;
    private PdfDocumentDescriptor firstItem;

    @Override
    public void start(Stage stage) throws Exception {
        victim = new SelectionTable(MODULE, true, true, new SelectionTableColumn<?>[] { new LoadingColumn(MODULE),
                FileColumn.NAME, LongColumn.SIZE, IntColumn.PAGES, LongColumn.LAST_MODIFIED, new PageRangesColumn() });
        victim.setId("victim");
        firstItem = populate();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @After
    public void tearDown() {
        type(KeyCode.ESCAPE);
    }

    @Test
    public void fallbackRequest() {
        HitTestListener<SetDestinationRequest> listener = new HitTestListener<>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertTrue(event.isFallback());
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
        clickOn("temp.pdf");
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
        clickOn("temp.pdf").press(KeyCode.CONTROL);
        eventStudio().add(SelectionChangedEvent.class, listener, MODULE);
        clickOn("temp3.pdf");
        release(KeyCode.CONTROL);
        assertTrue(listener.isHit());
    }

    @Test
    public void itemsAdded() {
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        assertEquals(4, victim.getItems().size());
        verify(listener).onEvent(any());
    }

    @Test
    public void onSaveWorkspace() {
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("4", data.get("victiminput.size"));
        assertThat(data.get("victiminput.0"), Matchers.endsWith("temp.pdf"));
        assertThat(data.get("victiminput.1"), Matchers.endsWith("®¯°±²³要选择需要转换的文.pdf"));
        assertThat(data.get("victiminput.2"), Matchers.endsWith("temp3.pdf"));
        assertThat(data.get("victiminput.3"), Matchers.endsWith("temp4.pdf"));
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
    @Category(NoHeadless.class)
    public void onSaveWorkspaceEncryptedPwdStored() {
        new DefaultUserContext().setBooleanPreference(BooleanUserPreference.SAVE_PWD_IN_WORKSPACE, true);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        });
        WaitForAsyncUtils.waitForFxEvents();
        clickOn(".glyph-icon");
        write("pwd").clickOn(DefaultI18nContext.getInstance().i18n("Unlock"));
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals(EncryptionUtils.encrypt("pwd"), data.get("victiminput.password.enc0"));
    }

    @Test
    public void onSaveWorkspaceEncryptedNoPwdStored() {
        new DefaultUserContext().setBooleanPreference(BooleanUserPreference.SAVE_PWD_IN_WORKSPACE, false);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        });
        WaitForAsyncUtils.waitForFxEvents();
        clickOn(".glyph-icon");
        write("pwd").clickOn(DefaultI18nContext.getInstance().i18n("Unlock"));
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(isBlank(data.get("victiminput.password.enc0")));
        assertTrue(isBlank(data.get("victiminput.password.0")));
    }

    @Test
    public void restoreStateFromPwdBackwardCompatible() {
        eventStudio().clear();
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
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
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
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
        verifyThat(victim, hasTableCell("1"));
        verifyThat(victim, hasTableCell("2"));
        verifyThat(victim, hasTableCell("3"));
        verifyThat(victim, hasTableCell("4"));
        verifyThat(victim, not(hasTableCell("5")));
    }

    @Test
    public void clear() {
        clickOn("temp.pdf");
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            eventStudio().broadcast(new ClearModuleEvent(), MODULE);
        });
        assertTrue(victim.getSelectionModel().getSelectedIndices().isEmpty());
    }

    @Test
    @Category(NoHeadless.class)
    public void encryptedThrowsRequest() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        });
        WaitForAsyncUtils.waitForFxEvents();
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        clickOn(".glyph-icon");
        write("pwd").clickOn(DefaultI18nContext.getInstance().i18n("Unlock"));
        verify(listener, times(2)).onEvent(any());
    }

    @Test
    public void clearSelectionByclickOn() {
        clickOn("temp.pdf");
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
        Listener<SelectionChangedEvent> listener = mock(Listener.class);
        ArgumentCaptor<SelectionChangedEvent> captor = ArgumentCaptor.forClass(SelectionChangedEvent.class);
        eventStudio().add(SelectionChangedEvent.class, listener, MODULE);
        press(KeyCode.CONTROL).clickOn("temp.pdf");
        assertTrue(victim.getSelectionModel().getSelectedIndices().isEmpty());
        release(KeyCode.CONTROL);
        verify(listener).onEvent(captor.capture());
        assertTrue(captor.getValue().isClearSelection());
    }

    @Test
    @Category(NoHeadless.class)
    public void removeByContextMenu() {
        rightClickOn("temp.pdf");
        clickOn(DefaultI18nContext.getInstance().i18n("Remove"));
        assertEquals(3, victim.getItems().size());
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
    }

    @Test
    public void removeMultiple() {
        clickOn("temp.pdf").press(KeyCode.CONTROL).clickOn("temp3.pdf").release(KeyCode.CONTROL);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            eventStudio().broadcast(new RemoveSelectedEvent(), MODULE);
        });
        assertEquals(2, victim.getItems().size());
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
    }

    @Test
    public void removeRelease() {
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        assertTrue(item.isPresent());
        clickOn("temp.pdf");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            eventStudio().broadcast(new RemoveSelectedEvent(), MODULE);
        });
        assertFalse(item.get().descriptor().hasReferences());
    }

    @Test
    @Category(NoHeadless.class)
    public void clearInvalidatesDuplicatedItems() {
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        rightClickOn("temp.pdf");
        clickOn(DefaultI18nContext.getInstance().i18n("Duplicate"));
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            eventStudio().broadcast(new ClearModuleEvent(), MODULE);
        });
        assertFalse(item.get().descriptor().hasReferences());
    }

    @Test
    @Category(NoHeadless.class)
    public void duplicate() {
        rightClickOn("temp.pdf");
        clickOn(DefaultI18nContext.getInstance().i18n("Duplicate"));
        assertEquals(2,
                victim.getItems().stream().filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).count());
    }

    @Test
    public void moveSelected() {
        clickOn("temp.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 0);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            eventStudio().broadcast(new MoveSelectedEvent(MoveType.DOWN), MODULE);
        });
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 1);
    }

    @Test
    @Category(NoHeadless.class)
    public void moveDownByContextMenu() {
        rightClickOn("temp.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 0);
        clickOn(DefaultI18nContext.getInstance().i18n("Move Down"));
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 1);
    }

    @Test
    @Category(NoHeadless.class)
    public void moveBottomByContextMenu() {
        rightClickOn("temp.pdf");
        assertEquals(0, victim.getSelectionModel().getSelectedIndex());
        clickOn(DefaultI18nContext.getInstance().i18n("Move to Bottom"));
        assertEquals(3, victim.getSelectionModel().getSelectedIndex());
    }

    @Test
    @Category(NoHeadless.class)
    public void moveUpByContextMenu() {
        rightClickOn("temp3.pdf");
        assertEquals(2, victim.getSelectionModel().getSelectedIndex());
        clickOn(DefaultI18nContext.getInstance().i18n("Move Up"));
        assertEquals(1, victim.getSelectionModel().getSelectedIndex());
    }

    @Test
    @Category(NoHeadless.class)
    public void moveTopByContextMenu() {
        rightClickOn("temp3.pdf");
        assertEquals(2, victim.getSelectionModel().getSelectedIndex());
        clickOn(DefaultI18nContext.getInstance().i18n("Move to Top"));
        assertEquals(0, victim.getSelectionModel().getSelectedIndex());
    }

    @Test
    @Category(NoHeadless.class)
    public void copy() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> Clipboard.getSystemClipboard().clear());
        rightClickOn("temp.pdf");
        clickOn(DefaultI18nContext.getInstance().i18n("Copy to clipboard"));
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> assertFalse(
                StringUtils.isEmpty(Clipboard.getSystemClipboard().getContent(DataFormat.PLAIN_TEXT).toString())));
    }

    @Test
    @Category(NoHeadless.class)
    public void pageRangesForAllByContextMenu() {
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        assertTrue(item.isPresent());
        item.get().pageSelection.set("2-4");
        rightClickOn("temp.pdf");
        clickOn(DefaultI18nContext.getInstance().i18n("Set as range for all"));
        victim.getItems().stream().forEach(i -> {
            assertEquals("2-4", i.pageSelection.get());
        });
    }

    @Test
    @Category(NoHeadless.class)
    public void setDestinationByContextMenu() {
        HitTestListener<SetDestinationRequest> listener = new HitTestListener<>();
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        rightClickOn("temp3.pdf");
        HitTestListener<SetDestinationRequest> notFallbackListener = new HitTestListener<>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertFalse(event.isFallback());
            }
        };
        eventStudio().add(SetDestinationRequest.class, notFallbackListener, MODULE);
        clickOn(DefaultI18nContext.getInstance().i18n("Set destination"));
        assertTrue(listener.isHit());
        assertTrue(notFallbackListener.isHit());
    }

    @Test
    @Category(NoHeadless.class)
    public void openByContextMenu() {
        HitTestListener<OpenFileRequest> listener = new HitTestListener<>();
        eventStudio().add(OpenFileRequest.class, listener);
        rightClickOn("temp3.pdf");
        clickOn(DefaultI18nContext.getInstance().i18n("Open"));
        assertTrue(listener.isHit());
    }

    @Test
    @Category(NoHeadless.class)
    public void openFolderByContextMenu() {
        HitTestListener<OpenFileRequest> listener = new HitTestListener<>();
        eventStudio().add(OpenFileRequest.class, listener);
        rightClickOn("temp3.pdf");
        clickOn(DefaultI18nContext.getInstance().i18n("Open Folder"));
        assertTrue(listener.isHit());
    }

    @Test
    @Category(NoHeadless.class)
    public void infoByContextMenu() {
        Listener<ShowPdfDescriptorRequest> listener = mock(Listener.class);
        eventStudio().add(ShowPdfDescriptorRequest.class, listener);
        rightClickOn("temp3.pdf");
        clickOn(DefaultI18nContext.getInstance().i18n("Document properties"));
        verify(listener, timeout(2000)).onEvent(any());
    }

    @Test
    @Category(NoWindows.class)
    public void iconsAreShown() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED));
        WaitForAsyncUtils.waitForFxEvents();
        Text icon = lookup(".glyph-icon").queryAs(Text.class);
        assertEquals(PdfDescriptorLoadingStatus.REQUESTED.getIcon().unicode(), icon.getText());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING));
        WaitForAsyncUtils.waitForFxEvents();
        icon = lookup(".glyph-icon").queryAs(Text.class);
        assertEquals(PdfDescriptorLoadingStatus.LOADING.getIcon().unicode(), icon.getText());
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
        clickOn(".glyph-icon");
        verify(listener).onEvent(any());
    }

    @Test
    public void logEventOnclickOn() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED));
        WaitForAsyncUtils.waitForFxEvents();
        Text icon = lookup(".glyph-icon").queryAs(Text.class);
        assertEquals(PdfDescriptorLoadingStatus.REQUESTED.getIcon().unicode(), icon.getText());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING));
        WaitForAsyncUtils.waitForFxEvents();
        icon = lookup(".glyph-icon").queryAs(Text.class);
        assertEquals(PdfDescriptorLoadingStatus.LOADING.getIcon().unicode(), icon.getText());
    }

    @Test
    @Category(NoHeadless.class)
    @Ignore("TODO")
    public void editCommitOnFocusLost() {
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        assertTrue(item.isPresent());
        item.get().pageSelection.set("2");
        WaitForAsyncUtils.waitForFxEvents();
        clickOn("2").type(KeyCode.ENTER, KeyCode.DIGIT5);
        clickOn("temp4.pdf");
        WaitForAsyncUtils.waitForFxEvents();
        assertEquals(item.get().pageSelection.get(), "5");
    }

    private PdfDocumentDescriptor populate() throws Exception {
        File file = folder.newFile("temp.pdf");
        File file2 = folder.newFile("®¯°±²³要选择需要转换的文.pdf");
        File file3 = folder.newFile("temp3.pdf");
        File file4 = folder.newFile("temp4.pdf");
        PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(MODULE);
        PdfDocumentDescriptor ret = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        loadEvent.add(ret);
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file2));
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file3));
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file4));
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            eventStudio().broadcast(loadEvent, MODULE);
        });
        return ret;
    }
}
