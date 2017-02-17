/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/ago/2014
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
package org.pdfsam.ui.selection.multiple;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.loadui.testfx.Assertions.verifyThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.hamcrest.Matchers;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.mockito.ArgumentCaptor;
import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
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
import org.sejda.eventstudio.Listener;

import javafx.scene.Parent;
import javafx.scene.input.KeyCode;
import javafx.scene.text.Text;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class SelectionTableTest extends GuiTest {
    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Override
    protected Parent getRootNode() {
        SelectionTable victim = new SelectionTable(MODULE, true, true,
                new SelectionTableColumn<?>[] { new LoadingColumn(MODULE), FileColumn.NAME, LongColumn.SIZE,
                        IntColumn.PAGES, LongColumn.LAST_MODIFIED, new PageRangesColumn() });
        victim.setId("victim");
        return victim;
    }

    @After
    public void tearDown() {
        type(KeyCode.ESCAPE);
    }

    @Test
    public void fallbackRequest() throws Exception {
        HitTestListener<SetDestinationRequest> listener = new HitTestListener<SetDestinationRequest>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertTrue(event.isFallback());
            }
        };
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        populate();
        assertTrue(listener.isHit());
    }

    @Test
    public void select() throws Exception {
        HitTestListener<SelectionChangedEvent> listener = new HitTestListener<SelectionChangedEvent>() {
            @Override
            public void onEvent(SelectionChangedEvent event) {
                super.onEvent(event);
                assertTrue(event.isSingleSelection());
            }
        };
        eventStudio().add(SelectionChangedEvent.class, listener, MODULE);
        populate();
        click("temp.pdf");
        assertTrue(listener.isHit());
    }

    @Test
    public void multipleSelect() throws Exception {
        HitTestListener<SelectionChangedEvent> listener = new HitTestListener<SelectionChangedEvent>() {
            @Override
            public void onEvent(SelectionChangedEvent event) {
                super.onEvent(event);
                assertFalse(event.isSingleSelection());
            }
        };
        populate();
        click("temp.pdf").press(KeyCode.CONTROL);
        eventStudio().add(SelectionChangedEvent.class, listener, MODULE);
        click("temp3.pdf");
        release(KeyCode.CONTROL);
        assertTrue(listener.isHit());
    }

    @Test
    public void itemsAdded() throws Exception {
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        populate();
        SelectionTable victim = find("#victim");
        assertEquals(4, victim.getItems().size());
        verify(listener).onEvent(any());
    }

    @Test
    public void onSaveWorkspace() throws Exception {
        SelectionTable victim = find("#victim");
        populate();
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
        SelectionTable victim = find("#victim");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("0", data.get("victiminput.size"));
        assertNull(data.get("victiminput.0"));
    }

    @Test
    public void onSaveWorkspaceEncryptedPwdStored() throws Exception {
        new DefaultUserContext().setBooleanPreference(BooleanUserPreference.SAVE_PWD_IN_WORKSPACE, true);
        SelectionTable victim = find("#victim");
        PdfDocumentDescriptor firstItem = populate();
        FXTestUtils.invokeAndWait(() -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        }, 2);
        click(".glyph-icon");
        type("pwd").click(DefaultI18nContext.getInstance().i18n("Unlock"));
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("pwd", data.get("victiminput.password.0"));
    }

    @Test
    public void onSaveWorkspaceEncryptedNoPwdStored() throws Exception {
        new DefaultUserContext().setBooleanPreference(BooleanUserPreference.SAVE_PWD_IN_WORKSPACE, false);
        SelectionTable victim = find("#victim");
        PdfDocumentDescriptor firstItem = populate();
        FXTestUtils.invokeAndWait(() -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        }, 2);
        click(".glyph-icon");
        type("pwd").click(DefaultI18nContext.getInstance().i18n("Unlock"));
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(isBlank(data.get("victiminput.password.0")));
    }

    @Test
    public void restoreStateFrom() throws Exception {
        SelectionTable victim = find("#victim");
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        Map<String, String> data = new HashMap<>();
        data.put("victiminput.size", "2");
        data.put("victiminput.0", "chuck.pdf");
        data.put("victiminput.password.0", "pwd");
        data.put("victiminput.range.0", "1-10");
        data.put("victiminput.step.0", "4");
        data.put("victiminput.reverse.0", "true");
        data.put("victiminput.1", "norris.pdf");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
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
    public void restoreStateFromEmpty() throws Exception {
        SelectionTable victim = find("#victim");
        populate();
        Map<String, String> data = new HashMap<>();
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertTrue(victim.getItems().isEmpty());
    }

    @Test
    public void restoreStateFromSizeZero() throws Exception {
        SelectionTable victim = find("#victim");
        populate();
        Map<String, String> data = new HashMap<>();
        data.put("victiminput.size", "0");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertTrue(victim.getItems().isEmpty());
    }

    @Test
    public void clear() throws Exception {
        populate();
        click("temp.pdf");
        SelectionTable victim = find("#victim");
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(new ClearModuleEvent(), MODULE);
        }, 2);
        assertTrue(victim.getSelectionModel().getSelectedIndices().isEmpty());
    }

    @Test
    public void clearSelectionByClick() throws Exception {
        populate();
        click("temp.pdf");
        SelectionTable victim = find("#victim");
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
        Listener<SelectionChangedEvent> listener = mock(Listener.class);
        ArgumentCaptor<SelectionChangedEvent> captor = ArgumentCaptor.forClass(SelectionChangedEvent.class);
        eventStudio().add(SelectionChangedEvent.class, listener, MODULE);
        press(KeyCode.CONTROL).click("temp.pdf");
        assertTrue(victim.getSelectionModel().getSelectedIndices().isEmpty());
        release(KeyCode.CONTROL);
        verify(listener).onEvent(captor.capture());
        assertTrue(captor.getValue().isClearSelection());
    }

    @Test
    public void removeByContextMenu() throws Exception {
        populate();
        rightClick("temp.pdf");
        click(DefaultI18nContext.getInstance().i18n("Remove"));
        SelectionTable victim = find("#victim");
        assertEquals(3, victim.getItems().size());
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
    }

    @Test
    @Ignore("Fails on CI server")
    // TODO
    public void removeMultiple() throws Exception {
        populate();
        click("temp.pdf").press(KeyCode.CONTROL).click("temp3.pdf").release(KeyCode.CONTROL);
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(new RemoveSelectedEvent(), MODULE);
        }, 2);
        SelectionTable victim = find("#victim");
        assertEquals(2, victim.getItems().size());
        assertEquals(1, victim.getSelectionModel().getSelectedIndices().size());
    }

    @Test
    public void removeRelease() throws Exception {
        populate();
        SelectionTable victim = find("#victim");
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        assertTrue(item.isPresent());
        click("temp.pdf");
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(new RemoveSelectedEvent(), MODULE);
        }, 2);
        assertFalse(item.get().descriptor().hasReferences());
    }

    @Test
    public void clearInvalidatesDuplicatedItems() throws Exception {
        populate();
        SelectionTable victim = find("#victim");
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        rightClick("temp.pdf");
        click(DefaultI18nContext.getInstance().i18n("Duplicate"));
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(new ClearModuleEvent(), MODULE);
        }, 2);
        assertFalse(item.get().descriptor().hasReferences());
    }

    @Test
    public void duplicate() throws Exception {
        populate();
        rightClick("temp.pdf");
        click(DefaultI18nContext.getInstance().i18n("Duplicate"));
        SelectionTable victim = find("#victim");
        assertEquals(2,
                victim.getItems().stream().filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).count());
    }

    @Test
    public void moveSelected() throws Exception {
        populate();
        click("temp.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 0);
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(new MoveSelectedEvent(MoveType.DOWN), MODULE);
        }, 2);
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 1);
    }

    @Test
    public void moveDownByContextMenu() throws Exception {
        populate();
        rightClick("temp.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 0);
        click(DefaultI18nContext.getInstance().i18n("Move Down"));
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 1);
    }

    @Test
    public void moveBottomByContextMenu() throws Exception {
        populate();
        rightClick("temp.pdf");
        SelectionTable victim = find("#victim");
        assertEquals(0, victim.getSelectionModel().getSelectedIndex());
        click(DefaultI18nContext.getInstance().i18n("Move to Bottom"));
        assertEquals(3, victim.getSelectionModel().getSelectedIndex());
    }

    @Test
    public void moveUpByContextMenu() throws Exception {
        populate();
        rightClick("temp3.pdf");
        SelectionTable victim = find("#victim");
        assertEquals(2, victim.getSelectionModel().getSelectedIndex());
        click(DefaultI18nContext.getInstance().i18n("Move Up"));
        assertEquals(1, victim.getSelectionModel().getSelectedIndex());
    }

    @Test
    public void moveTopByContextMenu() throws Exception {
        populate();
        rightClick("temp3.pdf");
        SelectionTable victim = find("#victim");
        assertEquals(2, victim.getSelectionModel().getSelectedIndex());
        click(DefaultI18nContext.getInstance().i18n("Move to Top"));
        assertEquals(0, victim.getSelectionModel().getSelectedIndex());
    }

    @Test
    public void setDestinationByContextMenu() throws Exception {
        HitTestListener<SetDestinationRequest> listener = new HitTestListener<>();
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        populate();
        rightClick("temp3.pdf");
        HitTestListener<SetDestinationRequest> notFallbackListener = new HitTestListener<SetDestinationRequest>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertFalse(event.isFallback());
            }
        };
        eventStudio().add(SetDestinationRequest.class, notFallbackListener, MODULE);
        click(DefaultI18nContext.getInstance().i18n("Set destination"));
        assertTrue(listener.isHit());
        assertTrue(notFallbackListener.isHit());
    }

    @Test
    public void openByContextMenu() throws Exception {
        HitTestListener<OpenFileRequest> listener = new HitTestListener<>();
        eventStudio().add(OpenFileRequest.class, listener);
        populate();
        rightClick("temp3.pdf");
        click(DefaultI18nContext.getInstance().i18n("Open"));
        assertTrue(listener.isHit());
    }

    @Test
    public void openFolderByContextMenu() throws Exception {
        HitTestListener<OpenFileRequest> listener = new HitTestListener<>();
        eventStudio().add(OpenFileRequest.class, listener);
        populate();
        rightClick("temp3.pdf");
        click(DefaultI18nContext.getInstance().i18n("Open Folder"));
        assertTrue(listener.isHit());
    }

    @Test
    public void infoByContextMenu() throws Exception {
        Listener<ShowPdfDescriptorRequest> listener = mock(Listener.class);
        eventStudio().add(ShowPdfDescriptorRequest.class, listener);
        populate();
        rightClick("temp3.pdf");
        click(DefaultI18nContext.getInstance().i18n("Document properties"));
        verify(listener, timeout(2000)).onEvent(any());
    }

    @Test
    public void iconsAreShown() throws Exception {
        PdfDocumentDescriptor firstItem = populate();
        FXTestUtils.invokeAndWait(() -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED), 2);
        Text icon = find(".glyph-icon");
        assertEquals(PdfDescriptorLoadingStatus.REQUESTED.getIcon().characterToString(), icon.getText());
        FXTestUtils.invokeAndWait(() -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING), 2);
        icon = find(".glyph-icon");
        assertEquals(PdfDescriptorLoadingStatus.LOADING.getIcon().characterToString(), icon.getText());
    }

    @Test
    public void clickWithErrorsShowsLogStage() throws Exception {
        PdfDocumentDescriptor firstItem = populate();
        FXTestUtils.invokeAndWait(() -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.WITH_ERRORS);
        }, 2);
        Listener<ShowStageRequest> listener = mock(Listener.class);
        eventStudio().add(ShowStageRequest.class, listener, "LogStage");
        click(".glyph-icon");
        verify(listener).onEvent(any());
    }

    @Test
    public void clickEncryptedThrowsRequest() throws Exception {
        PdfDocumentDescriptor firstItem = populate();
        FXTestUtils.invokeAndWait(() -> {
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            firstItem.moveStatusTo(PdfDescriptorLoadingStatus.ENCRYPTED);
        }, 2);
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        click(".glyph-icon");
        type("pwd").click(DefaultI18nContext.getInstance().i18n("Unlock"));
        verify(listener, times(2)).onEvent(any());
    }

    @Test
    public void logEventOnClick() throws Exception {
        PdfDocumentDescriptor firstItem = populate();
        FXTestUtils.invokeAndWait(() -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED), 2);
        Text icon = find(".glyph-icon");
        assertEquals(PdfDescriptorLoadingStatus.REQUESTED.getIcon().characterToString(), icon.getText());
        FXTestUtils.invokeAndWait(() -> firstItem.moveStatusTo(PdfDescriptorLoadingStatus.LOADING), 2);
        icon = find(".glyph-icon");
        assertEquals(PdfDescriptorLoadingStatus.LOADING.getIcon().characterToString(), icon.getText());
    }

    @Test
    @Ignore("Fails on Travis")
    public void editCommitOnFocusLost() throws Exception {
        populate();
        SelectionTable victim = find("#victim");
        Optional<SelectionTableRowData> item = victim.getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        assertTrue(item.isPresent());
        item.get().pageSelection.set("2");
        Thread.sleep(1000);
        click("2").type(KeyCode.ENTER, KeyCode.DIGIT5);
        click("temp4.pdf");
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
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(loadEvent, MODULE);
        }, 2);
        return ret;
    }
}
