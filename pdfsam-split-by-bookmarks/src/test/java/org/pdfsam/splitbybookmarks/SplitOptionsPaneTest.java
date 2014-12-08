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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import javafx.scene.Parent;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.test.ClearEventStudioRule;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class SplitOptionsPaneTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule();

    private SplitByGoToActionLevelParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(SplitByGoToActionLevelParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        return new SplitOptionsPane();
    }

    @Test
    public void applyLevel() throws Exception {
        SplitOptionsPane victim = find(".pdfsam-container");
        click("#bookmarksLevel").type('3').push(KeyCode.ENTER);
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(onError, never()).accept(anyString());
        verify(builder).level(3);
        verify(builder, never()).regexp(anyString());
    }

    @Test
    public void applyRegexp() throws Exception {
        SplitOptionsPane victim = find(".pdfsam-container");
        click("#bookmarksLevel").type('3').push(KeyCode.ENTER);
        click("#bookmarksRegexp").type("Chuck");
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(onError, never()).accept(anyString());
        verify(builder).level(3);
        verify(builder).regexp("Chuck");
    }

    @Test
    public void emptyLevel() throws Exception {
        SplitOptionsPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(onError).accept(anyString());
        verify(builder, never()).level(anyInt());
        verify(builder, never()).regexp(anyString());
    }

    @Test
    public void saveState() {
        SplitOptionsPane victim = find(".pdfsam-container");
        click("#bookmarksLevel").type('3').push(KeyCode.ENTER);
        click("#bookmarksRegexp").type("Chuck");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("Chuck", data.get("regexp"));
        assertEquals("3", data.get("levelCombo.selected"));
    }

    @Test
    public void restoreState() {
        SplitOptionsPane victim = find(".pdfsam-container");
        Map<String, String> data = new HashMap<>();
        data.put("regexp", "Chuck");
        data.put("levelCombo.selected", "2");
        victim.restoreStateFrom(data);
        TextField field = find("#bookmarksRegexp");
        assertEquals("Chuck", field.getText());
        BookmarksLevelComboBox levelCombo = find("#bookmarksLevel");
        assertEquals("2", levelCombo.getSelectionModel().getSelectedItem());
    }
}
