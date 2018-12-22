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
package org.pdfsam.splitbybookmarks;

import static org.hamcrest.Matchers.hasItems;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.pdfsam.NoHeadless;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class BookmarksLevelComboBoxTest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule();
    private SplitByOutlineLevelParametersBuilder builder;
    private Consumer<String> onError;
    private SortedSet<Integer> validLevels = new TreeSet<>(Arrays.asList(2, 3, 4, 5, 6, 7, 10));
    private BookmarksLevelComboBox victim;

    @Before
    public void setUp() {
        builder = mock(SplitByOutlineLevelParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    public void start(Stage stage) {
        victim = new BookmarksLevelComboBox();
        Scene scene = new Scene(new HBox(victim, new Button("Focus")));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Category(NoHeadless.class)
    public void validateOnChange() {
        victim.setValidBookmarkLevels(new TreeSet<>(Arrays.asList(300)));
        victim.setValidBookmarkLevels(validLevels);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        clickOn(victim).push(KeyCode.ALT, KeyCode.DOWN).clickOn("3");
        assertEquals(ValidationState.VALID, victim.getValidationState());
    }

    @Test
    public void invalidIntegerValue() {
        victim.setValidBookmarkLevels(validLevels);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        clickOn(victim).write("Chuck").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, victim.getValidationState());
        Arrays.stream(Style.INVALID.css()).forEach((s) -> assertFalse(lookup("." + s).tryQuery().isEmpty()));
    }

    @Test
    public void validValueOnFocusLost() {
        victim.setValidBookmarkLevels(validLevels);
        clickOn(victim).write("10").push(KeyCode.TAB);
        assertEquals("10", victim.getValue());
        assertEquals(ValidationState.VALID, victim.getValidationState());
    }

    @Test
    public void invalidValueOnFocusLost() {
        victim.setValidBookmarkLevels(validLevels);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        clickOn(victim).write("Chuck").push(KeyCode.TAB);
        assertEquals(ValidationState.INVALID, victim.getValidationState());
        Arrays.stream(Style.INVALID.css()).forEach((s) -> assertFalse(lookup("." + s).tryQuery().isEmpty()));
    }

    @Test
    public void invalidRangeValue() {
        victim.setValidBookmarkLevels(validLevels);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        clickOn(victim).write("40").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, victim.getValidationState());
        Arrays.stream(Style.INVALID.css()).forEach((s) -> assertFalse(lookup("." + s).tryQuery().isEmpty()));
    }

    @Test
    public void invalidNoMaxBookmarksSet() {
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        clickOn(victim).type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        victim.apply(builder, onError);
        assertEquals(ValidationState.INVALID, victim.getValidationState());
        Arrays.stream(Style.INVALID.css()).forEach((s) -> assertFalse(lookup("." + s).tryQuery().isEmpty()));
    }

    @Test
    public void invalidApply() {
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        victim.setValidBookmarkLevels(validLevels);
        clickOn(victim).write("Chuck").push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).level(anyInt());
    }

    @Test
    public void validApply() {
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        victim.setValidBookmarkLevels(validLevels);
        clickOn(victim).type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).level(eq(3));
    }

    @Test
    @Category(NoHeadless.class)
    public void saveState() {
        victim.setValidBookmarkLevels(validLevels);
        clickOn(victim).push(KeyCode.ALT, KeyCode.DOWN).clickOn("2");
        Map<String, String> data = new HashMap<>();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.saveStateTo(data));
        assertEquals("2,3,4,5,6,7,10", data.get("levelCombo.levels"));
        assertEquals("2", data.get("levelCombo.selected"));
    }

    @Test
    public void saveStateEmptySelection() {
        victim.setValidBookmarkLevels(validLevels);
        Map<String, String> data = new HashMap<>();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.saveStateTo(data));
        assertEquals("", data.get("levelCombo.selected"));
    }

    @Test
    public void restoreState() {
        victim.setValidBookmarkLevels(new TreeSet<>(Arrays.asList(40, 50)));
        Map<String, String> data = new HashMap<>();
        data.put("levelCombo.levels", "2,3,5,6,7,10");
        data.put("levelCombo.selected", "2");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals("2", victim.getValue());
        assertThat(victim.getItems(), hasItems("2", "3", "5", "6", "7", "10"));
    }

    @Test
    public void restoreStateBackwardCompatible() {
        victim.setValidBookmarkLevels(new TreeSet<>(Arrays.asList(40, 50)));
        Map<String, String> data = new HashMap<>();
        data.put("levelCombo.max", "3");
        data.put("levelCombo.selected", "2");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals("2", victim.getValue());
        assertThat(victim.getItems(), hasItems("1", "2", "3"));
    }

    @Test
    public void restoreStateEmptySelected() {
        victim.setValidBookmarkLevels(validLevels);
        Map<String, String> data = new HashMap<>();
        data.put("levelCombo.selected", "");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals("", victim.getValue());
    }

    @Test
    public void restoreStateNullSelected() {
        victim.setValidBookmarkLevels(validLevels);
        Map<String, String> data = new HashMap<>();
        data.put("levelCombo.selected", null);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals("", victim.getValue());
    }
}
