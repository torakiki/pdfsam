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
package org.pdfsam.ui.module;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Arrays;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.NoHeadless;
import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleInputOutputType;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestModule;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.commons.OpenFileRequest;
import org.pdfsam.ui.commons.SetActiveModuleRequest;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.task.NotifiableTaskMetadata;
import org.sejda.model.task.Task;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.scene.text.Text;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class OpenButtonTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule cearEventStudio = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder temp = new TemporaryFolder();
    private Module module = new DefaultPriorityTestModule();
    private OpenButton victim;

    @Override
    public void start(Stage stage) {
        victim = new OpenButton("moduleId", ModuleInputOutputType.SINGLE_PDF);
        victim.initModules(Arrays.asList(module));
        victim.setMaxHeight(30);
        victim.setVisible(true);
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void openFileClick() throws Exception {
        File file = temp.newFile();
        FileTaskOutput output = new FileTaskOutput(file);
        TestListener listener = new TestListener(file);
        eventStudio().add(listener);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        clickOn(victim);
        assertTrue(listener.isHit());
        assertTrue(listener.equal);
    }

    @Test
    public void openDirectoryClick() throws Exception {
        File dir = temp.newFolder();
        DirectoryTaskOutput output = new DirectoryTaskOutput(dir);
        TestListener listener = new TestListener(dir);
        eventStudio().add(listener);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        clickOn(victim);
        assertTrue(listener.isHit());
        assertTrue(listener.equal);
    }

    @Test
    public void openSingleFileDirectoryDestinationClick() throws Exception {
        File file = temp.newFile();
        DirectoryTaskOutput output = new DirectoryTaskOutput(file.getParentFile());
        TestListener listener = new TestListener(file);
        eventStudio().add(listener);
        NotifiableTaskMetadata taskMetadata = new NotifiableTaskMetadata(mock(Task.class));
        taskMetadata.addTaskOutput(file);
        eventStudio().broadcast(new TaskExecutionCompletedEvent(12, taskMetadata), "moduleId");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        clickOn(victim);
        assertTrue(listener.isHit());
        assertTrue(listener.equal);
    }

    @Test
    public void fileDestination() {
        FileTaskOutput output = mock(FileTaskOutput.class);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        verify(output).getDestination();
        Text icon = lookup(".glyph-icon").queryAs(Text.class);
        assertEquals(FontAwesomeIcon.FILE_PDF_ALT.unicode(), icon.getText());
    }

    @Test
    public void directoryDestination() {
        DirectoryTaskOutput output = mock(DirectoryTaskOutput.class);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        verify(output).getDestination();
        Text icon = lookup(".glyph-icon").queryAs(Text.class);
        assertEquals(MaterialDesignIcon.FOLDER_OUTLINE.unicode(), icon.getText());
    }

    @Test
    public void fileOrDirDestination() {
        DirectoryTaskOutput output = mock(DirectoryTaskOutput.class);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.dispatch(output));
        verify(output).getDestination();
        Text icon = lookup(".glyph-icon").queryAs(Text.class);
        assertEquals(MaterialDesignIcon.FOLDER_OUTLINE.unicode(), icon.getText());
    }

    @Test
    @Category(NoHeadless.class)
    public void sendToModuleClick() throws Exception {
        File file = temp.newFile();
        NotifiableTaskMetadata taskMetadata = new NotifiableTaskMetadata(mock(Task.class));
        taskMetadata.addTaskOutput(file);
        TaskExecutionCompletedEvent event = new TaskExecutionCompletedEvent(10, taskMetadata);
        HitTestListener<ClearModuleEvent> clear = new HitTestListener<>();
        eventStudio().add(ClearModuleEvent.class, clear, module.id());
        HitTestListener<PdfLoadRequestEvent> load = new HitTestListener<>();
        eventStudio().add(PdfLoadRequestEvent.class, load, module.id());
        HitTestListener<SetActiveModuleRequest> active = new HitTestListener<>();
        eventStudio().add(SetActiveModuleRequest.class, active);
        eventStudio().broadcast(event, "moduleId");
        clickOn(".arrow-button").clickOn(module.descriptor().getName());
        assertTrue(clear.isHit());
        assertTrue(active.isHit());
        assertTrue(load.isHit());
    }

    private static class TestListener extends HitTestListener<OpenFileRequest> {
        private File destination;
        boolean equal = false;

        private TestListener(File destination) {
            this.destination = destination;
        }

        @Override
        public void onEvent(OpenFileRequest event) {
            super.onEvent(event);
            equal = destination.equals(event.getFile());
        }
    }

}
