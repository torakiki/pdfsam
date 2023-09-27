/*
 * This file is part of the PDF Split And Merge source code
 * Created on 11/set/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.tools.splitbybookmarks;

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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
@Tag("NoHeadless")
public class SplitOptionsPaneTest {

    private SplitByOutlineLevelParametersBuilder builder;
    private Consumer<String> onError;
    private final SortedSet<Integer> validLevels = new TreeSet<>(Arrays.asList(2, 3, 4, 5, 6, 7, 10));
    private SplitOptionsPane victim;
    private FxRobot robot;

    @BeforeEach
    public void setUp() {
        builder = mock(SplitByOutlineLevelParametersBuilder.class);
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
    public void applyLevel() {
        BookmarksLevelComboBox combo = robot.lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        combo.setValidBookmarkLevels(validLevels);
        robot.clickOn("#bookmarksLevel").type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).level(3);
        verify(builder, never()).regexp(anyString());
    }

    @Test
    public void applyRegexp() {
        BookmarksLevelComboBox combo = robot.lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        combo.setValidBookmarkLevels(validLevels);
        robot.clickOn("#bookmarksLevel").type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        robot.clickOn("#bookmarksRegexp").write("Chuck");
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).level(3);
        verify(builder).regexp("Chuck");
    }

    @Test
    public void emptyLevel() {
        BookmarksLevelComboBox combo = robot.lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        combo.setValidBookmarkLevels(validLevels);
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).level(anyInt());
        verify(builder, never()).regexp(anyString());
    }

    @Test
    public void saveState() {
        BookmarksLevelComboBox combo = robot.lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        combo.setValidBookmarkLevels(validLevels);
        robot.clickOn("#bookmarksLevel").type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        robot.clickOn("#bookmarksRegexp").write("Chuck");
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
        TextInputControl field = robot.lookup("#bookmarksRegexp").queryTextInputControl();
        assertEquals("Chuck", field.getText());
        BookmarksLevelComboBox levelCombo = robot.lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        assertEquals(6, levelCombo.getItems().size());
        assertEquals("2", levelCombo.getSelectionModel().getSelectedItem());
    }

    @Test
    public void reset() {
        BookmarksLevelComboBox combo = robot.lookup("#bookmarksLevel").queryAs(BookmarksLevelComboBox.class);
        combo.setValidBookmarkLevels(validLevels);
        robot.clickOn("#bookmarksLevel").type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        robot.clickOn("#bookmarksRegexp").write("Chuck");
        victim.setValidBookmarkLevels(validLevels);
        robot.clickOn(combo).type(KeyCode.DIGIT3).push(KeyCode.ENTER);
        assertEquals("3", combo.getValue());
        assertThat(combo.getItems()).containsOnly("2", "3", "4", "5", "6", "7", "10");
        TextInputControl field = robot.lookup("#bookmarksRegexp").queryTextInputControl();
        assertEquals("Chuck", field.getText());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertNull(combo.getValue());
        assertTrue(combo.getItems().isEmpty());
        assertEquals("", field.getText());
    }
}
