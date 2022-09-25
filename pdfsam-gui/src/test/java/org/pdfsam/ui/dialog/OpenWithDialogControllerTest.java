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

import static org.mockito.Mockito.mock;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.tool.Tool;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.HighPriorityTestTool;
import org.pdfsam.ui.InputPdfArgumentsLoadRequest;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class OpenWithDialogControllerTest extends ApplicationTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private Tool tool1 = new HighPriorityTestTool();
    private Tool tool2 = new DefaultPriorityTestTool();
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule(tool1.id(), tool2.id());
    private Button button;

    @Override
    public void start(Stage stage) {
        StylesConfig styles = mock(StylesConfig.class);
        List<Tool> modulesMap = new ArrayList<>();
        modulesMap.add(tool1);
        modulesMap.add(tool2);
        new OpenWithDialogController(new OpenWithDialog(styles, modulesMap));
        button = new Button("show");
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void singleArg() throws IOException {
        InputPdfArgumentsLoadRequest event = new InputPdfArgumentsLoadRequest();
        event.pdfs.add(Paths.get(folder.newFile().getAbsolutePath()));
        button.setOnAction(a -> eventStudio().broadcast(event));
        clickOn("show");
        clickOn(tool2.descriptor().getName());
    }

    @Test
    public void multipleArgs() throws IOException {
        InputPdfArgumentsLoadRequest event = new InputPdfArgumentsLoadRequest();
        event.pdfs.add(Paths.get(folder.newFile().getAbsolutePath()));
        event.pdfs.add(Paths.get(folder.newFile().getAbsolutePath()));
        button.setOnAction(a -> eventStudio().broadcast(event));
        clickOn("show");
        clickOn(tool1.descriptor().getName());
    }
}
