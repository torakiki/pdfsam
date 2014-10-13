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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.loadui.testfx.Assertions.verifyThat;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Optional;

import javafx.scene.Parent;
import javafx.scene.input.KeyCode;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.loadui.testfx.utils.TestUtils;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.commons.OpenFileRequest;
import org.pdfsam.ui.commons.SetDestinationRequest;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.pdfsam.ui.selection.multiple.move.MoveSelectedEvent;
import org.pdfsam.ui.selection.multiple.move.MoveType;

import de.jensd.fx.fontawesome.AwesomeIcon;

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
        SelectionTable victim = new SelectionTable(MODULE, new SelectionTableColumn<?>[] {
                new LoadingStatusColumn(MODULE), FileColumn.NAME, LongColumn.SIZE, LongColumn.PAGES,
                LongColumn.LAST_MODIFIED, StringColumn.PAGE_SELECTION });
        victim.setId("victim");
        return victim;
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
        HitTestListener<PdfLoadRequestEvent> listener = new HitTestListener<>();
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        populate();
        verifyThat("#victim", (SelectionTable n) -> n.getItems().size() == 3);
        assertTrue(listener.isHit());
    }

    @Test
    public void clear() throws Exception {
        itemsAdded();
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(new ClearSelectionTableEvent(), MODULE);
        }, 2);
        verifyThat("#victim", (SelectionTable n) -> n.getItems().isEmpty());
    }

    @Test
    public void clearByClick() throws Exception {
        populate();
        click("temp.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndices().size() == 1);
        HitTestListener<SelectionChangedEvent> listener = new HitTestListener<SelectionChangedEvent>() {
            @Override
            public void onEvent(SelectionChangedEvent event) {
                super.onEvent(event);
                assertTrue(event.isClearSelection());
            }
        };
        eventStudio().add(SelectionChangedEvent.class, listener, MODULE);
        press(KeyCode.CONTROL).click("temp.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndices().isEmpty());
        release(KeyCode.CONTROL);
        assertTrue(listener.isHit());
    }

    @Test
    public void removeByContextMenu() throws Exception {
        populate();
        rightClick("temp.pdf");
        click(AwesomeIcon.MINUS_SQUARE_ALT.toString());
        verifyThat("#victim", (SelectionTable n) -> n.getItems().size() == 2);
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndices().size() == 1);
    }

    @Test
    public void removeMultiple() throws Exception {
        populate();
        click("temp.pdf").press(KeyCode.CONTROL).click("temp3.pdf").release(KeyCode.CONTROL);
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(new RemoveSelectedEvent(), MODULE);
        }, 2);
        verifyThat("#victim", (SelectionTable n) -> n.getItems().size() == 1);
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndices().isEmpty());
    }

    @Test
    public void removeInvalidates() throws Exception {
        populate();
        SelectionTable victim = find("#victim");
        Optional<SelectionTableRowData> item = victim.getItems().stream().filter(i -> i.getFileName() != "temp.pdf")
                .findFirst();
        assertTrue(item.isPresent());
        click("temp.pdf");
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(new RemoveSelectedEvent(), MODULE);
        }, 2);
        assertTrue(item.get().isInvalid());
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
        click(AwesomeIcon.ANGLE_DOWN.toString());
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 1);
    }

    @Test
    public void moveBottomByContextMenu() throws Exception {
        populate();
        rightClick("temp.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 0);
        click(AwesomeIcon.ANGLE_DOUBLE_DOWN.toString());
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 2);
    }

    @Test
    public void moveUpByContextMenu() throws Exception {
        populate();
        rightClick("temp3.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 2);
        click(AwesomeIcon.ANGLE_UP.toString());
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 1);
    }

    @Test
    public void moveTopByContextMenu() throws Exception {
        populate();
        rightClick("temp3.pdf");
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 2);
        click(AwesomeIcon.ANGLE_DOUBLE_UP.toString());
        verifyThat("#victim", (SelectionTable n) -> n.getSelectionModel().getSelectedIndex() == 0);
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
        click(AwesomeIcon.FILE_PDF_ALT.toString());
        assertTrue(listener.isHit());
        assertTrue(notFallbackListener.isHit());
    }

    @Test
    public void openByContextMenu() throws Exception {
        HitTestListener<OpenFileRequest> listener = new HitTestListener<>();
        eventStudio().add(OpenFileRequest.class, listener);
        populate();
        rightClick("temp3.pdf");
        click(AwesomeIcon.FILE_ALT.toString());
        assertTrue(listener.isHit());
    }

    @Test
    public void openFolderByContextMenu() throws Exception {
        HitTestListener<OpenFileRequest> listener = new HitTestListener<>();
        eventStudio().add(OpenFileRequest.class, listener);
        populate();
        rightClick("temp3.pdf");
        click(AwesomeIcon.FOLDER_ALTPEN.toString());
        assertTrue(listener.isHit());
    }

    @Test
    public void infoByContextMenu() throws Exception {
        HitTestListener<ShowPdfDescriptorRequest> listener = new HitTestListener<>();
        eventStudio().add(ShowPdfDescriptorRequest.class, listener);
        populate();
        rightClick("temp3.pdf");
        click(AwesomeIcon.INFO.toString());
        TestUtils.awaitCondition(listener::isHit, 2);
    }

    private void populate() throws Exception {
        File file = folder.newFile("temp.pdf");
        File file2 = folder.newFile("temp2.pdf");
        File file3 = folder.newFile("temp3.pdf");
        PdfLoadRequestEvent<SelectionTableRowData> loadEvent = new PdfLoadRequestEvent<>(MODULE);
        loadEvent.add(new SelectionTableRowData(file));
        loadEvent.add(new SelectionTableRowData(file2));
        loadEvent.add(new SelectionTableRowData(file3));
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(loadEvent, MODULE);
        }, 2);
    }
}
