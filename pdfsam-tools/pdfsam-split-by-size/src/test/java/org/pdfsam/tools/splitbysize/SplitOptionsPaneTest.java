/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.tools.splitbysize;

import javafx.scene.Scene;
import javafx.scene.control.TextInputControl;
import javafx.scene.input.KeyCode;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
@Tag("NoHeadless")
public class SplitOptionsPaneTest {

    private SplitBySizeParametersBuilder builder;
    private Consumer<String> onError;
    private SplitOptionsPane victim;
    private FxRobot robot;

    @BeforeEach
    public void setUp() {
        builder = mock(SplitBySizeParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Start
    public void start(Stage stage) {
        victim = new SplitOptionsPane();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void apply() {
        robot.clickOn("#sizeField").write("30").push(KeyCode.ENTER);
        robot.clickOn("#unit" + SizeUnit.MEGABYTE.symbol());
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).size(eq(30 * 1024 * 1024L));
    }

    @Test
    public void applyError() {
        robot.clickOn("#sizeField").write("Chuck").push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).size(anyLong());
    }

    @Test
    public void saveState() {
        robot.clickOn("#sizeField").write("3000").push(KeyCode.ENTER);
        robot.clickOn("#unit" + SizeUnit.KILOBYTE.symbol());
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("3000", data.get("size"));
        assertFalse(Boolean.parseBoolean(data.get(SizeUnit.MEGABYTE.toString())));
        assertTrue(Boolean.parseBoolean(data.get(SizeUnit.KILOBYTE.toString())));
    }

    @Test
    public void restoreState() {
        SizeUnitRadio kilo = robot.lookup("#unit" + SizeUnit.KILOBYTE.symbol()).queryAs(SizeUnitRadio.class);
        SizeUnitRadio mega = robot.lookup("#unit" + SizeUnit.MEGABYTE.symbol()).queryAs(SizeUnitRadio.class);
        Map<String, String> data = new HashMap<>();
        data.put("size", "100");
        data.put(SizeUnit.MEGABYTE.toString(), Boolean.TRUE.toString());
        victim.restoreStateFrom(data);
        TextInputControl field = robot.lookup("#sizeField").queryTextInputControl();
        assertEquals("100", field.getText());
        assertTrue(mega.isSelected());
        assertFalse(kilo.isSelected());
    }

    @Test
    public void reset() {
        SizeUnitRadio kilo = robot.lookup("#unit" + SizeUnit.KILOBYTE.symbol()).queryAs(SizeUnitRadio.class);
        robot.clickOn("#sizeField").write("100").push(KeyCode.ENTER);
        robot.clickOn("#unit" + SizeUnit.KILOBYTE.symbol());
        TextInputControl field = robot.lookup("#sizeField").queryTextInputControl();
        assertEquals("100", field.getText());
        assertTrue(kilo.isSelected());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals("", field.getText());
        assertFalse(kilo.isSelected());
    }
}
