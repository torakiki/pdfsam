/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/lug/2014
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
package org.pdfsam.ui.module;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.OutputStream;
import java.util.Arrays;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleInputOutputType;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestModule;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.commons.ClearSelectionEvent;
import org.pdfsam.ui.commons.OpenFileRequest;
import org.pdfsam.ui.commons.SetActiveModuleRequest;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.output.StreamTaskOutput;
import org.sejda.model.task.NotifiableTaskMetadata;
import org.sejda.model.task.Task;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.scene.Parent;
import javafx.scene.text.Text;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class OpenButtonTest extends GuiTest {

    @Rule
    public ClearEventStudioRule cearEventStudio = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder temp = new TemporaryFolder();
    private Module module = new DefaultPriorityTestModule();

    @Override
    protected Parent getRootNode() {
        OpenButton button = new OpenButton("moduleId", ModuleInputOutputType.SINGLE_PDF);
        button.initModules(Arrays.asList(module));
        button.setMaxHeight(30);
        button.setVisible(true);
        return button;
    }

    @Test
    public void openClick() throws Exception {
        File file = temp.newFile();
        FileTaskOutput output = new FileTaskOutput(file);
        TestListener listener = new TestListener(file);
        OpenButton victim = find(".footer-open-button");
        eventStudio().add(listener);
        FXTestUtils.invokeAndWait(() -> victim.dispatch(output), 1);
        click(victim);
        assertTrue(listener.isHit());
    }

    @Test
    public void fileDestination() throws Exception {
        OpenButton victim = find(".footer-open-button");
        FileTaskOutput output = mock(FileTaskOutput.class);
        FXTestUtils.invokeAndWait(() -> victim.dispatch(output), 1);
        verify(output).getDestination();
        Text icon = find(".glyph-icon");
        assertEquals(FontAwesomeIcon.FILE_PDF_ALT.characterToString(), icon.getText());
    }

    @Test
    public void directoryDestination() throws Exception {
        OpenButton victim = find(".footer-open-button");
        DirectoryTaskOutput output = mock(DirectoryTaskOutput.class);
        FXTestUtils.invokeAndWait(() -> victim.dispatch(output), 1);
        verify(output).getDestination();
        Text icon = find(".glyph-icon");
        assertEquals(MaterialDesignIcon.FOLDER_OUTLINE.characterToString(), icon.getText());
    }

    @Test(expected = IllegalArgumentException.class)
    public void streamDestination() {
        OpenButton victim = find(".footer-open-button");
        victim.dispatch(new StreamTaskOutput(mock(OutputStream.class)));
    }

    @Test
    public void sendToModuleClick() throws Exception {
        File file = temp.newFile();
        NotifiableTaskMetadata taskMetadata = new NotifiableTaskMetadata(mock(Task.class));
        taskMetadata.addTaskOutput(file);
        TaskExecutionCompletedEvent event = new TaskExecutionCompletedEvent(10, taskMetadata);
        HitTestListener<ClearSelectionEvent> clear = new HitTestListener<>();
        eventStudio().add(ClearSelectionEvent.class, clear, module.id());
        HitTestListener<PdfLoadRequestEvent> load = new HitTestListener<>();
        eventStudio().add(PdfLoadRequestEvent.class, load, module.id());
        HitTestListener<SetActiveModuleRequest> active = new HitTestListener<>();
        eventStudio().add(SetActiveModuleRequest.class, active);
        eventStudio().broadcast(event, "moduleId");
        click(".arrow-button").click(module.descriptor().getName());
        assertTrue(clear.isHit());
        assertTrue(active.isHit());
        assertTrue(load.isHit());
    }


    private static class TestListener extends HitTestListener<OpenFileRequest> {
        private File destination;

        private TestListener(File destination) {
            this.destination = destination;
        }

        @Override
        public void onEvent(OpenFileRequest event) {
            super.onEvent(event);
            assertEquals(destination, event.getFile());
        }
    }

}
