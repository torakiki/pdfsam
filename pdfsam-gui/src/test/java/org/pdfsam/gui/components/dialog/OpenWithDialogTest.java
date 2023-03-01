/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12 ago 2016
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.gui.components.dialog;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.InputPdfArgumentsLoadRequest;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
@Tag("NoHeadless")
public class OpenWithDialogTest {

    private final Tool tool = new DefaultPriorityTestTool();
    @RegisterExtension
    public ClearEventStudioExtension clearEventStudio = new ClearEventStudioExtension(tool.id());
    private Button button;
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        new OpenWithDialogController(new OpenWithDialog(List.of(tool), stage));
        button = new Button("show");
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void singleArg(@TempDir Path folder) throws IOException {
        Listener<ClearToolRequest> clearListener = mock(Listener.class);
        eventStudio().add(ClearToolRequest.class, clearListener, tool.id());
        Listener<SetActiveContentItemRequest> activeModuleListener = mock(Listener.class);
        eventStudio().add(SetActiveContentItemRequest.class, activeModuleListener);
        Listener<PdfLoadRequest> loadRequestListener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, loadRequestListener, tool.id());

        var event = new InputPdfArgumentsLoadRequest(List.of(Files.createTempFile(folder, null, null)));
        button.setOnAction(a -> eventStudio().broadcast(event));
        robot.clickOn("show");
        robot.clickOn(tool.descriptor().name());
        ArgumentCaptor<ClearToolRequest> captor = ArgumentCaptor.forClass(ClearToolRequest.class);
        verify(clearListener).onEvent(captor.capture());
        assertFalse(captor.getValue().askConfirmation());
        verify(activeModuleListener).onEvent(any());
        verify(loadRequestListener).onEvent(any());
    }

}