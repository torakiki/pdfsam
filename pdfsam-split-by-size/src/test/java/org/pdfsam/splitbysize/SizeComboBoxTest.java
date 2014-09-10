/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
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
package org.pdfsam.splitbysize;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.function.Consumer;

import javafx.scene.Parent;
import javafx.scene.input.KeyCode;

import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class SizeComboBoxTest extends GuiTest {

    private SplitBySizeParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(SplitBySizeParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        SizeComboBox victim = new SizeComboBox();
        victim.setId("victim");
        return victim;
    }

    @Test
    public void validateOnChange() {
        SizeComboBox victim = find("#victim");
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click("#victim").push(KeyCode.ALT, KeyCode.DOWN).click("3 MB");
        assertEquals(ValidationState.VALID, victim.getValidationState());
    }

    @Test
    public void invalidState() {
        SizeComboBox victim = find("#victim");
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click("#victim").click().click().type("A3 MB").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, victim.getValidationState());
        Arrays.stream(Style.INVALID.css()).forEach((s) -> exists("." + s));
    }

    @Test
    public void invalidApply() throws Exception {
        SizeComboBox victim = find("#victim");
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click("#victim").click().click().type("A3 MB").push(KeyCode.ENTER);
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 1);
        verify(onError).accept(anyString());
        verify(builder, never()).size(anyLong());
    }

    @Test
    public void validKB() throws Exception {
        SizeComboBox victim = find("#victim");
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click("#victim").click().click().type("3 KB").push(KeyCode.ENTER);
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 1);
        verify(onError, never()).accept(anyString());
        verify(builder).size(eq(3072L));
    }

    @Test
    public void validMB() throws Exception {
        SizeComboBox victim = find("#victim");
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click("#victim").click().click().type("3 MB").push(KeyCode.ENTER);
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 1);
        verify(onError, never()).accept(anyString());
        verify(builder).size(eq(3145728L));
    }
}
