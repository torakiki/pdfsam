/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12 ago 2016
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
package org.pdfsam.ui.dialog;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.module.Module;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestModule;
import org.pdfsam.ui.InputPdfArgumentsLoadRequest;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.commons.SetActiveModuleRequest;
import org.sejda.eventstudio.Listener;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class OpenWithDialogTest extends ApplicationTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private Module module = new DefaultPriorityTestModule();
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule(module.id());
    private Button button;

    @Override
    public void start(Stage stage) {
        StylesConfig styles = mock(StylesConfig.class);
        List<Module> modulesMap = new ArrayList<>();
        modulesMap.add(module);
        new OpenWithDialogController(new OpenWithDialog(styles, modulesMap));
        button = new Button("show");
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void singleArg() throws IOException {
        Listener<ClearModuleEvent> clearListener = mock(Listener.class);
        eventStudio().add(ClearModuleEvent.class, clearListener, module.id());
        Listener<SetActiveModuleRequest> activeModuleListener = mock(Listener.class);
        eventStudio().add(SetActiveModuleRequest.class, activeModuleListener);
        Listener<PdfLoadRequestEvent> loadRequestListener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, loadRequestListener, module.id());

        InputPdfArgumentsLoadRequest event = new InputPdfArgumentsLoadRequest();
        event.pdfs.add(Paths.get(folder.newFile().getAbsolutePath()));
        button.setOnAction(a -> eventStudio().broadcast(event));
        clickOn("show");
        clickOn(module.descriptor().getName());
        verify(clearListener).onEvent(any());
        verify(activeModuleListener).onEvent(any());
        verify(loadRequestListener).onEvent(any());
    }

}