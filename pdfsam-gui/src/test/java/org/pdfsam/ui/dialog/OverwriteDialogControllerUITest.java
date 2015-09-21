/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/ott/2014
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
package org.pdfsam.ui.dialog;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.IOException;
import java.util.Locale;

import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.MergeParameters;
import org.sejda.model.parameter.SimpleSplitParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;

import javafx.scene.Parent;
import javafx.scene.control.Button;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class OverwriteDialogControllerUITest extends GuiTest {
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @BeforeClass
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
    }

    @Override
    protected Parent getRootNode() {
        StylesConfig styles = mock(StylesConfig.class);
        OverwriteDialogController victim = new OverwriteDialogController(new OverwriteConfirmationDialog(styles));
        Button button = new Button("show");
        return button;
    }

    @Test
    public void cancelOnFileExists() throws IOException {
        MergeParameters parameters = new MergeParameters();
        parameters.setOverwrite(false);
        File file = folder.newFile();
        parameters.setOutput(new FileTaskOutput(file));
        Button button = find("show");
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequestEvent("id", parameters)));
        click("show");
        click(DefaultI18nContext.getInstance().i18n("Cancel"));
        assertFalse(parameters.isOverwrite());
    }

    @Test
    public void overwriteOnFileExists() throws IOException {
        MergeParameters parameters = new MergeParameters();
        parameters.setOverwrite(false);
        File file = folder.newFile();
        parameters.setOutput(new FileTaskOutput(file));
        Button button = find("show");
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequestEvent("id", parameters)));
        click("show");
        click(DefaultI18nContext.getInstance().i18n("Overwrite"));
        assertTrue(parameters.isOverwrite());
    }

    @Test
    public void cancelOnNotEmptyDir() throws IOException {
        SimpleSplitParameters parameters = new SimpleSplitParameters(PredefinedSetOfPages.ALL_PAGES);
        parameters.setOverwrite(false);
        folder.newFile();
        parameters.setOutput(new DirectoryTaskOutput(folder.getRoot()));
        Button button = find("show");
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequestEvent("id", parameters)));
        click("show");
        click(DefaultI18nContext.getInstance().i18n("Cancel"));
        assertFalse(parameters.isOverwrite());
    }

    @Test
    public void overwriteOnNotEmptyDir() throws IOException {
        SimpleSplitParameters parameters = new SimpleSplitParameters(PredefinedSetOfPages.ALL_PAGES);
        parameters.setOverwrite(false);
        folder.newFile();
        parameters.setOutput(new DirectoryTaskOutput(folder.getRoot()));
        Button button = find("show");
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequestEvent("id", parameters)));
        click("show");
        click(DefaultI18nContext.getInstance().i18n("Overwrite"));
        assertTrue(parameters.isOverwrite());
    }
}
