/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/set/2014
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

import javafx.scene.Parent;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.split.SplitByEveryRadioButton.SplitByEveryXPagesParametersBuilder;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.parameter.SplitByEveryXPagesParameters;
import org.sejda.model.pdf.PdfVersion;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class SplitByEveryRadioButtonTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private Consumer<String> onError;

    @Before
    public void setUp() {
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        ValidableTextField field = new ValidableTextField();
        field.setId("field");
        SplitByEveryRadioButton victim = new SplitByEveryRadioButton(field);
        victim.setId("victim");
        return new HBox(victim, field);
    }

    @Test
    public void valid() {
        ValidableTextField field = find("#field");
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        click(field).type("2").push(KeyCode.ENTER);
        assertEquals(ValidationState.VALID, field.getValidationState());
    }

    @Test
    public void invalid() {
        ValidableTextField field = find("#field");
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        click(field).type("Chuck").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, field.getValidationState());
    }

    @Test
    public void invalidBuilder() throws Exception {
        ValidableTextField field = find("#field");
        SplitByEveryRadioButton victim = find("#victim");
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        click(field).type("Chuck").push(KeyCode.ENTER);
        FXTestUtils.invokeAndWait(() -> victim.getBuilder(onError), 1);
        verify(onError).accept(anyString());
    }

    @Test
    public void builder() throws Exception {
        ValidableTextField field = find("#field");
        SplitByEveryRadioButton victim = find("#victim");
        assertEquals(ValidationState.NOT_VALIDATED, field.getValidationState());
        click(field).type("6").push(KeyCode.ENTER);
        final File file = folder.newFile("my.pdf");
        FXTestUtils.invokeAndWait(() -> {
            SplitByEveryXPagesParametersBuilder builder = victim.getBuilder(onError);
            builder.compress(true);
            DirectoryTaskOutput output = mock(DirectoryTaskOutput.class);
            builder.output(output);
            builder.overwrite(true);
            builder.prefix("prefix");
            PdfFileSource source = PdfFileSource.newInstanceNoPassword(file);
            builder.source(source);
            builder.version(PdfVersion.VERSION_1_7);
            SplitByEveryXPagesParameters params = builder.build();
            assertTrue(params.isCompress());
            assertTrue(params.isOverwrite());
            assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
            assertThat(params.getPages(20), contains(6, 12, 18));
            assertEquals("prefix", params.getOutputPrefix());
            assertEquals(output, params.getOutput());
            assertEquals(source, params.getSource());
            verify(onError, never()).accept(anyString());
        }, 2);
    }

    @Test
    public void saveStateSelected() {
        SplitByEveryRadioButton victim = find("#victim");
        click(victim);
        click("#field").type("chuck");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(Boolean.valueOf(data.get("splitByEvery")));
        assertEquals("chuck", data.get("splitByEvery.field"));
    }

    @Test
    public void saveStateNotSelected() {
        SplitByEveryRadioButton victim = find("#victim");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertFalse(Boolean.valueOf(data.get("splitByEvery")));
        assertEquals("", data.get("splitByEvery.field"));
    }

    @Test
    public void restoreState() throws Exception {
        SplitByEveryRadioButton victim = find("#victim");
        ValidableTextField field = find("#field");
        Map<String, String> data = new HashMap<>();
        data.put("splitByEvery", Boolean.TRUE.toString());
        data.put("splitByEvery.field", "chuck");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertTrue(victim.isSelected());
        assertEquals("chuck", field.getText());
    }
}
