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
import org.pdfsam.test.ClearEventStudioRule;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.TextInputControl;
import javafx.scene.input.KeyCode;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class SplitOptionsPaneTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule();
    private SplitBySizeParametersBuilder builder;
    private Consumer<String> onError;
    private SplitOptionsPane victim;

    @Before
    public void setUp() {
        builder = mock(SplitBySizeParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    public void start(Stage stage) {
        victim = new SplitOptionsPane();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void apply() {
        clickOn("#sizeField").write("30").push(KeyCode.ENTER);
        clickOn("#unit" + SizeUnit.MEGABYTE.symbol());
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).size(eq(30 * 1024 * 1024L));
    }

    @Test
    public void applyError() {
        clickOn("#sizeField").write("Chuck").push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).size(anyLong());
    }

    @Test
    public void saveState() {
        clickOn("#sizeField").write("3000").push(KeyCode.ENTER);
        clickOn("#unit" + SizeUnit.KILOBYTE.symbol());
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("3000", data.get("size"));
        assertFalse(Boolean.valueOf(data.get(SizeUnit.MEGABYTE.toString())));
        assertTrue(Boolean.valueOf(data.get(SizeUnit.KILOBYTE.toString())));
    }

    @Test
    public void restoreState() {
        SizeUnitRadio kilo = lookup("#unit" + SizeUnit.KILOBYTE.symbol()).queryAs(SizeUnitRadio.class);
        SizeUnitRadio mega = lookup("#unit" + SizeUnit.MEGABYTE.symbol()).queryAs(SizeUnitRadio.class);
        Map<String, String> data = new HashMap<>();
        data.put("size", "100");
        data.put(SizeUnit.MEGABYTE.toString(), Boolean.TRUE.toString());
        victim.restoreStateFrom(data);
        TextInputControl field = lookup("#sizeField").queryTextInputControl();
        assertEquals("100", field.getText());
        assertTrue(mega.isSelected());
        assertFalse(kilo.isSelected());
    }
}
