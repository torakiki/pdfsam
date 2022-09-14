/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/ott/2014
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

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.IOException;
import java.util.Locale;

import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.NoHeadless;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.injector.Components;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.MergeParameters;
import org.sejda.model.parameter.SimpleSplitParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class OverwriteDialogControllerUITest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private Button button;

    @BeforeClass
    public static void setUp() {
        ((I18nContext) I18nContext.getInstance()).refresh(
                new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Override
    public void start(Stage stage) {
        Injector.start(new Config());
        button = new Button("show");
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Components({ OverwriteDialogController.class })
    static class Config {

        @Provides
        StylesConfig style() {
            return mock(StylesConfig.class);
        }

    }

    @Test
    public void cancelOnFileExists() throws IOException {
        MergeParameters parameters = new MergeParameters();
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.FAIL);
        File file = folder.newFile();
        parameters.setOutput(new FileTaskOutput(file));
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequestEvent("id", parameters)));
        clickOn(button);
        clickOn(I18nContext.getInstance().i18n("Cancel"));
        assertEquals(ExistingOutputPolicy.FAIL, parameters.getExistingOutputPolicy());
    }

    @Test
    @Category(NoHeadless.class)
    public void overwriteOnFileExists() throws IOException {
        MergeParameters parameters = new MergeParameters();
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.FAIL);
        File file = folder.newFile();
        parameters.setOutput(new FileTaskOutput(file));
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequestEvent("id", parameters)));
        clickOn(button);
        clickOn(I18nContext.getInstance().i18n("Overwrite"));
        assertEquals(ExistingOutputPolicy.OVERWRITE, parameters.getExistingOutputPolicy());
    }

    @Test
    @Category(NoHeadless.class)
    public void cancelOnNotEmptyDir() throws IOException {
        SimpleSplitParameters parameters = new SimpleSplitParameters(PredefinedSetOfPages.ALL_PAGES);
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.FAIL);
        folder.newFile();
        parameters.setOutput(FileOrDirectoryTaskOutput.directory(folder.getRoot()));
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequestEvent("id", parameters)));
        clickOn(button);
        clickOn(I18nContext.getInstance().i18n("Cancel"));
        assertEquals(ExistingOutputPolicy.FAIL, parameters.getExistingOutputPolicy());
    }

    @Test
    @Category(NoHeadless.class)
    public void overwriteOnNotEmptyDir() throws IOException {
        SimpleSplitParameters parameters = new SimpleSplitParameters(PredefinedSetOfPages.ALL_PAGES);
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.FAIL);
        folder.newFile();
        parameters.setOutput(FileOrDirectoryTaskOutput.directory(folder.getRoot()));
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequestEvent("id", parameters)));
        clickOn(button);
        clickOn(I18nContext.getInstance().i18n("Overwrite"));
        assertEquals(ExistingOutputPolicy.OVERWRITE, parameters.getExistingOutputPolicy());
    }
}
