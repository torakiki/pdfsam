/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
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
package org.pdfsam.splitbysize;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.Rule;
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
public class SplitOptionsPaneTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule();
    private SplitBySizeParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(SplitBySizeParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        return new SplitOptionsPane();
    }

    @Test
    public void apply() {
        SplitOptionsPane victim = find(".pdfsam-container");
        click("#sizeField").type("30").push(KeyCode.ENTER);
        click("#unit" + SizeUnit.MEGABYTE.symbol());
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).size(eq(30 * 1024 * 1024L));
    }

    @Test
    public void applyError() {
        SplitOptionsPane victim = find(".pdfsam-container");
        click("#sizeField").type("Chuck").push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).size(anyLong());
    }

    @Test
    public void saveState() {
        SplitOptionsPane victim = find(".pdfsam-container");
        click("#sizeField").type("3000").push(KeyCode.ENTER);
        click("#unit" + SizeUnit.MEGABYTE.symbol());
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("3000", data.get("size"));
        assertTrue(Boolean.valueOf(data.get(SizeUnit.MEGABYTE.toString())));
        assertFalse(Boolean.valueOf(data.get(SizeUnit.KILOBYTE.toString())));
    }

    @Test
    public void restoreState() throws Exception {
        SplitOptionsPane victim = find(".pdfsam-container");
        SizeUnitRadio kilo = find("#unit" + SizeUnit.KILOBYTE.symbol());
        SizeUnitRadio mega = find("#unit" + SizeUnit.MEGABYTE.symbol());
        Map<String, String> data = new HashMap<>();
        data.put("size", "100");
        data.put(SizeUnit.MEGABYTE.toString(), Boolean.TRUE.toString());
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        ValidableTextField field = find("#sizeField");
        assertEquals("100", field.getText());
        assertTrue(mega.isSelected());
        assertFalse(kilo.isSelected());
    }
}
