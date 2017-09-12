/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12 ago 2016
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.module.Module;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestModule;
import org.pdfsam.test.HighPriorityTestModule;
import org.pdfsam.ui.InputPdfArgumentsLoadRequest;

import javafx.scene.Parent;
import javafx.scene.control.Button;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class OpenWithDialogControllerTest extends GuiTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private Module module1 = new HighPriorityTestModule();
    private Module module2 = new DefaultPriorityTestModule();
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule(module1.id(), module2.id());

    @Override
    protected Parent getRootNode() {
        StylesConfig styles = mock(StylesConfig.class);
        List<Module> modulesMap = new ArrayList<>();
        modulesMap.add(module1);
        modulesMap.add(module2);
        new OpenWithDialogController(new OpenWithDialog(styles, modulesMap));
        Button button = new Button("show");
        return button;
    }

    @Test
    public void singleArg() throws IOException {
        Button button = find("show");
        InputPdfArgumentsLoadRequest event = new InputPdfArgumentsLoadRequest();
        event.pdfs.add(Paths.get(folder.newFile().getAbsolutePath()));
        button.setOnAction(a -> eventStudio().broadcast(event));
        click("show");
        click(module2.descriptor().getName());
    }

    @Test
    public void multipleArgs() throws IOException {
        Button button = find("show");
        InputPdfArgumentsLoadRequest event = new InputPdfArgumentsLoadRequest();
        event.pdfs.add(Paths.get(folder.newFile().getAbsolutePath()));
        event.pdfs.add(Paths.get(folder.newFile().getAbsolutePath()));
        button.setOnAction(a -> eventStudio().broadcast(event));
        click("show");
        click(module1.descriptor().getName());
    }
}
