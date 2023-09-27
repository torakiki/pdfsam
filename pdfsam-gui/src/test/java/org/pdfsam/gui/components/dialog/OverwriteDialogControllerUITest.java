/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/ott/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.MergeParameters;
import org.sejda.model.parameter.SimpleSplitParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
@Tag("NoHeadless")
public class OverwriteDialogControllerUITest {

    @TempDir
    public Path folder;
    private Button button;
    private FxRobot robot;

    @BeforeAll
    public static void setUp() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Start
    public void start(Stage stage) {
        new OverwriteDialogController(() -> new OverwriteConfirmationDialog(stage));
        button = new Button("show");
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void cancelOnFileExists() throws IOException {
        MergeParameters parameters = new MergeParameters();
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.FAIL);
        File file = Files.createTempFile(folder, null, null).toFile();
        parameters.setOutput(new FileTaskOutput(file));
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequest("id", parameters)));
        robot.clickOn(button);
        robot.clickOn(i18n().tr("Cancel"));
        assertEquals(ExistingOutputPolicy.FAIL, parameters.getExistingOutputPolicy());
    }

    @Test
    public void overwriteOnFileExists() throws IOException {
        MergeParameters parameters = new MergeParameters();
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.FAIL);
        File file = Files.createTempFile(folder, null, null).toFile();
        parameters.setOutput(new FileTaskOutput(file));
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequest("id", parameters)));
        robot.clickOn(button);
        robot.clickOn(i18n().tr("Overwrite"));
        assertEquals(ExistingOutputPolicy.OVERWRITE, parameters.getExistingOutputPolicy());
    }

    @Test
    public void cancelOnNotEmptyDir() throws IOException {
        SimpleSplitParameters parameters = new SimpleSplitParameters(PredefinedSetOfPages.ALL_PAGES);
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.FAIL);
        Files.createTempFile(folder, null, null);
        parameters.setOutput(FileOrDirectoryTaskOutput.directory(folder.getRoot().toFile()));
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequest("id", parameters)));
        robot.clickOn(button);
        robot.clickOn(i18n().tr("Cancel"));
        assertEquals(ExistingOutputPolicy.FAIL, parameters.getExistingOutputPolicy());
    }

    @Test
    public void overwriteOnNotEmptyDir() throws IOException {
        SimpleSplitParameters parameters = new SimpleSplitParameters(PredefinedSetOfPages.ALL_PAGES);
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.FAIL);
        Files.createTempFile(folder, null, null);
        parameters.setOutput(FileOrDirectoryTaskOutput.directory(folder.getRoot().toFile()));
        button.setOnAction(a -> eventStudio().broadcast(new TaskExecutionRequest("id", parameters)));
        robot.clickOn(button);
        robot.clickOn(i18n().tr("Overwrite"));
        assertEquals(ExistingOutputPolicy.OVERWRITE, parameters.getExistingOutputPolicy());
    }
}
