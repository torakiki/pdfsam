/*
 * This file is part of the PDF Split And Merge source code
 * Created on 25/lug/2014
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
package org.pdfsam.ui.components.tool;

import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.model.io.NativeOpenFileRequest;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolInputOutputType;
import org.pdfsam.model.ui.SetActiveToolRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.HitTestListener;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.task.NotifiableTaskMetadata;
import org.sejda.model.task.Task;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
public class OpenButtonTest {

    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension("moduleId");

    private final Tool tool = new DefaultPriorityTestTool();
    private OpenButton victim;
    private FxRobot robot;
    @Start
    public void start(Stage stage) {
        victim = new OpenButton("moduleId", ToolInputOutputType.SINGLE_PDF, Collections.singletonList(tool));
        victim.setMaxHeight(30);
        victim.setVisible(true);
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void openFileClick(@TempDir Path folder) throws Exception {
        File file = Files.createTempFile(folder, null, null).toFile();
        FileTaskOutput output = new FileTaskOutput(file);
        TestListener listener = new TestListener(file);
        eventStudio().add(listener);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        robot.clickOn(victim);
        assertTrue(listener.isHit());
        assertTrue(listener.equal);
    }

    @Test
    public void openDirectoryClick(@TempDir Path folder) throws Exception {
        File dir = Files.createTempDirectory(folder, null).toFile();
        DirectoryTaskOutput output = new DirectoryTaskOutput(dir);
        TestListener listener = new TestListener(dir);
        eventStudio().add(listener);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        robot.clickOn(victim);
        assertTrue(listener.isHit());
        assertTrue(listener.equal);
    }

    @Test
    public void openSingleFileDirectoryDestinationClick(@TempDir Path folder) throws Exception {
        File file = Files.createTempFile(folder, null, null).toFile();
        DirectoryTaskOutput output = new DirectoryTaskOutput(file.getParentFile());
        TestListener listener = new TestListener(file);
        eventStudio().add(listener);
        NotifiableTaskMetadata taskMetadata = new NotifiableTaskMetadata(mock(Task.class));
        taskMetadata.addTaskOutput(file);
        eventStudio().broadcast(new TaskExecutionCompletedEvent(12, taskMetadata), "moduleId");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        robot.clickOn(victim);
        assertTrue(listener.isHit());
        assertTrue(listener.equal);
    }

    @Test
    public void fileDestination() {
        FileTaskOutput output = mock(FileTaskOutput.class);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        verify(output).getDestination();
        Text icon = robot.lookup(".ikonli-font-icon").queryAs(Text.class);
        assertEquals(String.valueOf((char) UniconsLine.FILE_ALT.getCode()), icon.getText());
    }

    @Test
    public void directoryDestination() {
        DirectoryTaskOutput output = mock(DirectoryTaskOutput.class);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        verify(output).getDestination();
        Text icon = robot.lookup(".ikonli-font-icon").queryAs(Text.class);
        assertEquals(String.valueOf((char) UniconsLine.FOLDER_OPEN.getCode()), icon.getText());
    }

    @Test
    public void fileOrDirDestination() {
        DirectoryTaskOutput output = mock(DirectoryTaskOutput.class);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        verify(output).getDestination();
        Text icon = robot.lookup(".ikonli-font-icon").queryAs(Text.class);
        assertEquals(String.valueOf((char) UniconsLine.FOLDER_OPEN.getCode()), icon.getText());
    }

    @Test
    public void sendToModuleClick(@TempDir Path folder) throws Exception {
        File file = Files.createTempFile(folder, null, null).toFile();
        NotifiableTaskMetadata taskMetadata = new NotifiableTaskMetadata(mock(Task.class));
        taskMetadata.addTaskOutput(file);
        TaskExecutionCompletedEvent event = new TaskExecutionCompletedEvent(10, taskMetadata);
        HitTestListener<ClearToolRequest> clear = new HitTestListener<>();
        eventStudio().add(ClearToolRequest.class, clear, tool.id());
        HitTestListener<PdfLoadRequest> load = new HitTestListener<>();
        eventStudio().add(PdfLoadRequest.class, load, tool.id());
        HitTestListener<SetActiveToolRequest> active = new HitTestListener<>();
        eventStudio().add(SetActiveToolRequest.class, active);
        eventStudio().broadcast(event, "moduleId");
        robot.clickOn(".arrow-button").clickOn(tool.descriptor().name());
        assertTrue(clear.isHit());
        assertTrue(active.isHit());
        assertTrue(load.isHit());
    }

    private static class TestListener extends HitTestListener<NativeOpenFileRequest> {
        private final File destination;
        boolean equal = false;

        private TestListener(File destination) {
            this.destination = destination;
        }

        @Override
        public void onEvent(NativeOpenFileRequest event) {
            super.onEvent(event);
            equal = destination.equals(event.file());
        }
    }

}
