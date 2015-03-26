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
package org.pdfsam.splitbybookmarks;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import javafx.scene.Parent;
import javafx.scene.input.KeyCode;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class BookmarksLevelComboBoxTest extends GuiTest {
    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule();
    private SplitByOutlineLevelParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(SplitByOutlineLevelParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        BookmarksLevelComboBox victim = new BookmarksLevelComboBox();
        victim.setId("victim");
        return victim;
    }

    @Test
    public void validateOnChange() {
        BookmarksLevelComboBox victim = find("#victim");
        victim.setMaxBookmarkLevel(3);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click("#victim").push(KeyCode.ALT, KeyCode.DOWN).click("3");
        assertEquals(ValidationState.VALID, victim.getValidationState());
    }

    @Test
    public void invalidIntegerValue() {
        BookmarksLevelComboBox victim = find("#victim");
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click("#victim").type("Chuck").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, victim.getValidationState());
        Arrays.stream(Style.INVALID.css()).forEach((s) -> exists("." + s));
    }

    @Test
    public void invalidRangeValue() {
        BookmarksLevelComboBox victim = find("#victim");
        victim.setMaxBookmarkLevel(3);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click("#victim").type("4").push(KeyCode.ENTER);
        assertEquals(ValidationState.INVALID, victim.getValidationState());
        Arrays.stream(Style.INVALID.css()).forEach((s) -> exists("." + s));
    }

    @Test
    public void invalidApply() throws Exception {
        BookmarksLevelComboBox victim = find("#victim");
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click("#victim").type("Chuck").push(KeyCode.ENTER);
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 1);
        verify(onError).accept(anyString());
        verify(builder, never()).level(anyInt());
    }

    @Test
    public void validApply() throws Exception {
        BookmarksLevelComboBox victim = find("#victim");
        assertEquals(ValidationState.NOT_VALIDATED, victim.getValidationState());
        click("#victim").type("3").push(KeyCode.ENTER);
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 1);
        verify(onError, never()).accept(anyString());
        verify(builder).level(eq(3));
    }

    @Test
    public void saveState() {
        BookmarksLevelComboBox victim = find("#victim");
        victim.setMaxBookmarkLevel(3);
        click(victim).push(KeyCode.ALT, KeyCode.DOWN).click("2");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("3", data.get("levelCombo.max"));
        assertEquals("2", data.get("levelCombo.selected"));
    }

    @Test
    public void saveStateEmptySelection() {
        BookmarksLevelComboBox victim = find("#victim");
        victim.setMaxBookmarkLevel(3);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("", data.get("levelCombo.selected"));
    }

    @Test
    public void restoreState() throws Exception {
        BookmarksLevelComboBox victim = find("#victim");
        Map<String, String> data = new HashMap<>();
        data.put("levelCombo.max", "3");
        data.put("levelCombo.selected", "2");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertEquals("2", victim.getSelectionModel().getSelectedItem());
        assertEquals(3, victim.getItems().size());
    }
}
