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
package org.pdfsam.extract;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class ExtractOptionsPaneTest extends ApplicationTest {

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();

    private ExtractParametersBuilder builder;
    private Consumer<String> onError;
    private ExtractOptionsPane victim;

    @Before
    public void setUp() {
        builder = mock(ExtractParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    public void start(Stage stage) {
        victim = new ExtractOptionsPane();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validSteps() {
        clickOn("#extractRanges").type(KeyCode.DIGIT5).push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(builder).ranges(anySet());
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void applyError() {
        clickOn("#extractRanges").write("Chuck").push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).ranges(anySet());
    }

    @Test
    public void saveState() {
        clickOn("#extractRanges").write("30-100").push(KeyCode.ENTER);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("30-100", data.get("pages"));
    }

    @Test
    public void restoreState() {
        Map<String, String> data = new HashMap<>();
        data.put("pages", "100");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        ValidableTextField field = lookup("#extractRanges").queryAs(ValidableTextField.class);
        assertEquals("100", field.getText());
    }

    @Test
    public void reset() {
        clickOn("#extractRanges").type(KeyCode.DIGIT5).push(KeyCode.ENTER);
        ValidableTextField field = lookup("#extractRanges").queryAs(ValidableTextField.class);
        assertEquals("5", field.getText());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals("", field.getText());
    }
}
