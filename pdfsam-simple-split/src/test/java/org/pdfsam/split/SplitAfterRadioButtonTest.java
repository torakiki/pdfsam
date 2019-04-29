/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/set/2014
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
package org.pdfsam.split;

import static org.hamcrest.Matchers.contains;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.split.SplitAfterRadioButton.SplitByPageParametersBuilder;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.parameter.SplitByPagesParameters;
import org.sejda.model.pdf.PdfVersion;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class SplitAfterRadioButtonTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private Consumer<String> onError;
    private SplitAfterRadioButton victim;

    @Before
    public void setUp() {
        onError = mock(Consumer.class);
    }

    @Override
    public void start(Stage stage) {
        ValidableTextField field = new ValidableTextField();
        field.setId("field");
        victim = new SplitAfterRadioButton(field);
        victim.setId("victim");
        Scene scene = new Scene(new HBox(victim, field));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void valid() {
        ValidableTextField field = lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        clickOn(field).write("2,4,10").push(KeyCode.ENTER);
        assertEquals(ValidationState.VALID, field.getValidationState());
    }

    @Test
    public void validWhiteSpace() {
        ValidableTextField field = lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        clickOn(field).write("2, 4 ,10").push(KeyCode.ENTER);
        assertEquals(ValidationState.VALID, field.getValidationState());
    }

    @Test
    public void invalid() {
        ValidableTextField field = lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        clickOn(field).write("Chuck").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, field.getValidationState());
    }

    @Test
    public void invalidZero() {
        ValidableTextField field = lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        clickOn(field).write("0").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, field.getValidationState());
    }

    @Test
    public void invalidContainsZero() {
        ValidableTextField field = lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        clickOn(field).write("14,0").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, field.getValidationState());
    }

    @Test
    public void invalidBuilder() {
        ValidableTextField field = lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        clickOn(field).write("Chuck").push(KeyCode.ENTER);
        victim.getBuilder(onError);
        verify(onError).accept(anyString());
    }

    @Test
    public void builder() throws Exception {
        ValidableTextField field = lookup("#field").queryAs(ValidableTextField.class);
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        clickOn(field).write("1,10").push(KeyCode.ENTER);
        final File file = folder.newFile("my.pdf");
        SplitByPageParametersBuilder builder = victim.getBuilder(onError);
        builder.compress(true);
        FileOrDirectoryTaskOutput output = mock(FileOrDirectoryTaskOutput.class);
        builder.output(output);
        builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
        builder.prefix("prefix");
        PdfFileSource source = PdfFileSource.newInstanceNoPassword(file);
        builder.source(source);
        builder.version(PdfVersion.VERSION_1_7);
        SplitByPagesParameters params = builder.build();
        assertTrue(params.isCompress());
        assertEquals(ExistingOutputPolicy.OVERWRITE, params.getExistingOutputPolicy());
        assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
        assertThat(params.getPages(20), contains(1, 10));
        assertEquals("prefix", params.getOutputPrefix());
        assertEquals(output, params.getOutput());
        assertEquals(source, params.getSourceList().get(0));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void saveStateSelected() {
        clickOn(victim);
        clickOn("#field").write("chuck");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(Boolean.valueOf(data.get("splitAfter")));
        assertEquals("chuck", data.get("splitAfter.field"));
    }

    @Test
    public void saveStateNotSelected() {
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertFalse(Boolean.valueOf(data.get("splitAfter")));
        assertEquals("", data.get("splitAfter.field"));
    }

    @Test
    public void restoreState() {
        ValidableTextField field = lookup("#field").queryAs(ValidableTextField.class);
        Map<String, String> data = new HashMap<>();
        data.put("splitAfter", Boolean.TRUE.toString());
        data.put("splitAfter.field", "chuck");
        victim.restoreStateFrom(data);
        assertTrue(victim.isSelected());
        assertEquals("chuck", field.getText());
    }

    @Test
    public void reset() {
        ValidableTextField field = lookup("#field").queryAs(ValidableTextField.class);
        clickOn(field).type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        assertEquals("3", field.getText());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals("", field.getText());
    }
}
