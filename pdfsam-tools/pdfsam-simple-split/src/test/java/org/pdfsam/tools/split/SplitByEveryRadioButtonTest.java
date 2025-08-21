/*
 * This file is part of the PDF Split And Merge source code
 * Created on 11/set/2014
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
package org.pdfsam.tools.split;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.model.ui.workspace.WorkspaceData;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.support.FXValidationSupport.ValidationState;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.parameter.SplitByEveryXPagesParameters;
import org.sejda.model.pdf.PdfVersion;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * @author Andrea Vacondio
 *
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
@Tag("NoHeadless")
public class SplitByEveryRadioButtonTest {

    private Consumer<String> onError;
    private SplitByEveryRadioButton victim;
    @TempDir
    private Path folder;
    private FxRobot robot;

    @BeforeEach
    public void setUp() {
        onError = mock(Consumer.class);
    }

    @Start
    public void start(Stage stage) {
        ValidableTextField field = new ValidableTextField();
        field.setId("field");
        victim = new SplitByEveryRadioButton(field);
        victim.setId("victim");
        Scene scene = new Scene(new HBox(victim, field));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void valid() {
        ValidableTextField field = robot.lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        robot.clickOn(field).type(KeyCode.DIGIT2).push(KeyCode.ENTER);
        assertEquals(ValidationState.VALID, field.getValidationState());
    }

    @Test
    public void invalid() {
        ValidableTextField field = robot.lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        robot.clickOn(field).write("Chuck").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, field.getValidationState());
    }

    @Test
    public void invalidMaxPage() {
        ValidableTextField field = robot.lookup("#field").queryAs(ValidableTextField.class);
        victim.setMaxPages(10);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        robot.clickOn(field).write("10").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, field.getValidationState());
    }

    @Test
    public void invalidBuilder() {
        ValidableTextField field = robot.lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        robot.clickOn(field).write("Chuck").push(KeyCode.ENTER);
        victim.getBuilder(onError);
        verify(onError).accept(anyString());
    }

    @Test
    public void builder() throws Exception {
        ValidableTextField field = robot.lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        robot.clickOn(field).type(KeyCode.DIGIT6).push(KeyCode.ENTER);
        var file = Files.createTempFile(folder, null, ".pdf").toFile();
        SplitByEveryRadioButton.SplitByEveryXPagesParametersBuilder builder = victim.getBuilder(onError);
        builder.compress(true);
        FileOrDirectoryTaskOutput output = mock(FileOrDirectoryTaskOutput.class);
        builder.output(output);
        builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
        builder.prefix("prefix");
        PdfFileSource source = PdfFileSource.newInstanceNoPassword(file);
        builder.source(source);
        builder.version(PdfVersion.VERSION_1_7);
        SplitByEveryXPagesParameters params = builder.build();
        assertTrue(params.isCompress());
        assertEquals(ExistingOutputPolicy.OVERWRITE, params.getExistingOutputPolicy());
        assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
        assertThat(params.getPages(20)).containsOnly(6, 12, 18);
        assertEquals("prefix", params.getOutputPrefix());
        assertEquals(output, params.getOutput());
        assertEquals(source, params.getSourceList().get(0));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void saveStateSelected() {
        robot.clickOn(victim);
        robot.clickOn("#field").write("chuck");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(Boolean.parseBoolean(data.get("splitByEvery")));
        assertEquals("chuck", data.get("splitByEvery.field"));
    }

    @Test
    public void saveStateNotSelected() {
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertFalse(Boolean.parseBoolean(data.get("splitByEvery")));
        assertEquals("", data.get("splitByEvery.field"));
    }

    @Test
    public void restoreState() {
        ValidableTextField field = robot.lookup("#field").queryAs(ValidableTextField.class);
        WorkspaceData.ToolData data = new WorkspaceData.ToolData();
        data.setBoolean("splitByEvery", true);
        data.set("splitByEvery.field", "chuck");
        victim.restoreStateFrom(data);
        assertTrue(victim.isSelected());
        assertEquals("chuck", field.getText());
    }

    @Test
    public void reset() {
        ValidableTextField field = robot.lookup("#field").queryAs(ValidableTextField.class);
        robot.clickOn(field).type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        assertEquals("3", field.getText());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals("", field.getText());
    }
}
