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
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
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
import org.pdfsam.test.ClearEventStudioRule;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

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

    private SplitByOutlineLevelParametersBuilder builder;
    private Consumer<String> onError;
    private SortedSet<Integer> validLevels = new TreeSet<>(Arrays.asList(2, 3, 4, 5, 6, 7, 10));
    private SplitOptionsPane victim;

    @Before
    public void setUp() {
        builder = mock(SplitByOutlineLevelParametersBuilder.class);
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
    public void applyLevel() {
        BookmarksLevelComboBox combo = lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        combo.setValidBookmarkLevels(validLevels);
        clickOn("#bookmarksLevel").type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).level(3);
        verify(builder, never()).regexp(anyString());
    }

    @Test
    public void applyRegexp() {
        BookmarksLevelComboBox combo = lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        combo.setValidBookmarkLevels(validLevels);
        clickOn("#bookmarksLevel").type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        clickOn("#bookmarksRegexp").write("Chuck");
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).level(3);
        verify(builder).regexp("Chuck");
    }

    @Test
    public void emptyLevel() {
        BookmarksLevelComboBox combo = lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        combo.setValidBookmarkLevels(validLevels);
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).level(anyInt());
        verify(builder, never()).regexp(anyString());
    }

    @Test
    public void saveState() {
        BookmarksLevelComboBox combo = lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        combo.setValidBookmarkLevels(validLevels);
        clickOn("#bookmarksLevel").type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        clickOn("#bookmarksRegexp").write("Chuck");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("Chuck", data.get("regexp"));
        assertEquals("3", data.get("levelCombo.selected"));
        assertEquals("2,3,4,5,6,7,10", data.get("levelCombo.levels"));
    }

    @Test
    public void saveStateEmptyRegexp() {
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("", data.get("regexp"));
    }

    @Test
    public void restoreState() {
        Map<String, String> data = new HashMap<>();
        data.put("regexp", "Chuck");
        data.put("levelCombo.selected", "2");
        data.put("levelCombo.levels", "2,3,5,6,7,10");
        victim.restoreStateFrom(data);
        TextInputControl field = lookup("#bookmarksRegexp").queryTextInputControl();
        assertEquals("Chuck", field.getText());
        BookmarksLevelComboBox levelCombo = lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        assertEquals(6, levelCombo.getItems().size());
        assertEquals("2", levelCombo.getSelectionModel().getSelectedItem());
    }

    @Test
    public void reset() {
        BookmarksLevelComboBox combo = lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        combo.setValidBookmarkLevels(validLevels);
        clickOn("#bookmarksLevel").type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        clickOn("#bookmarksRegexp").write("Chuck");
        victim.setValidBookmarkLevels(validLevels);
        clickOn(combo).type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        assertEquals("3", combo.getValue());
        assertThat(combo.getItems(), hasItems("2", "3", "4", "5", "6", "7", "10"));
        TextInputControl field = lookup("#bookmarksRegexp").queryTextInputControl();
        assertEquals("Chuck", field.getText());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals(null, combo.getValue());
        assertTrue(combo.getItems().isEmpty());
        assertEquals("", field.getText());
    }
}
