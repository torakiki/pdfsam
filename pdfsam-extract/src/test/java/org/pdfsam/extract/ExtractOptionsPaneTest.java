/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
package org.pdfsam.extract;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anySet;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.ValidableTextField;

import javafx.scene.Parent;
import javafx.scene.input.KeyCode;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class ExtractOptionsPaneTest extends GuiTest {

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();

    private ExtractParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(ExtractParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        return new ExtractOptionsPane();
    }

    @Test
    public void validSteps() {
        ExtractOptionsPane victim = find(".pdfsam-container");
        click("#extractRanges").type('5').push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(builder).ranges(anySet());
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void applyError() {
        ExtractOptionsPane victim = find(".pdfsam-container");
        click("#extractRanges").type("Chuck").push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).ranges(anySet());
    }

    @Test
    public void saveState() {
        ExtractOptionsPane victim = find(".pdfsam-container");
        click("#extractRanges").type("30-100").push(KeyCode.ENTER);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("30-100", data.get("pages"));
    }

    @Test
    public void restoreState() throws Exception {
        ExtractOptionsPane victim = find(".pdfsam-container");
        Map<String, String> data = new HashMap<>();
        data.put("pages", "100");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        ValidableTextField field = find("#extractRanges");
        assertEquals("100", field.getText());
    }

    @Test
    public void reset() throws Exception {
        ExtractOptionsPane victim = find(".pdfsam-container");
        click("#extractRanges").type('5').push(KeyCode.ENTER);
        ValidableTextField field = (ValidableTextField) find("#extractRanges");
        assertEquals("5", field.getText());
        FXTestUtils.invokeAndWait(() -> victim.resetView(), 2);
        assertEquals("", field.getText());
    }
}
