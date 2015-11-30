/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/set/2014
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
package org.pdfsam.alternatemix;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
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
import javafx.scene.control.CheckBox;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class AlternateMixOptionsPaneTest extends GuiTest {

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();
    private AlternateMixParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(AlternateMixParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        return new AlternateMixOptionsPane();
    }

    @Test
    public void invalidFirstStep() throws Exception {
        click("#alternateMixFirstStep").type('f');
        AlternateMixOptionsPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(onError).accept(anyString());
    }

    @Test
    public void invalidSecondStep() throws Exception {
        click("#alternateMixSecondStep").type('f');
        AlternateMixOptionsPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(onError).accept(anyString());
    }

    @Test
    public void invalidMaxFirstStep() throws Exception {
        AlternateMixOptionsPane victim = find(".pdfsam-container");
        victim.setFirstDocumentMaxPages(10);
        doubleClick("#alternateMixFirstStep").type("30");
        doubleClick("#alternateMixSecondStep").type('2');
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(onError).accept(anyString());
        // tooltip to disapper
        Thread.sleep(5500);
    }

    @Test
    public void invalidMaxSecondStep() throws Exception {
        AlternateMixOptionsPane victim = find(".pdfsam-container");
        victim.setSecondDocumentMaxPages(10);
        doubleClick("#alternateMixFirstStep").type('2');
        doubleClick("#alternateMixSecondStep").type("30");
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(onError).accept(anyString());
        // tooltip to disapper
        Thread.sleep(5500);
    }

    @Test
    public void validSteps() throws Exception {
        doubleClick("#alternateMixFirstStep").type('3');
        doubleClick("#alternateMixSecondStep").type('2');
        AlternateMixOptionsPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(builder).stepFirst(3);
        verify(builder).stepSecond(2);
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void onSaveWorkspace() {
        doubleClick("#alternateMixFirstStep").type('3');
        doubleClick("#alternateMixSecondStep").type('2');
        Map<String, String> data = new HashMap<>();
        AlternateMixOptionsPane victim = find(".pdfsam-container");
        victim.saveStateTo(data);
        assertEquals("3", data.get("firstStep"));
        assertEquals("2", data.get("secondStep"));
        assertEquals(Boolean.TRUE.toString(), data.get("reverseSecond"));
        assertEquals(Boolean.FALSE.toString(), data.get("reverseFirst"));
    }

    @Test
    public void restoreStateFrom() throws Exception {
        CheckBox reverseFirst = find("#reverseFirst");
        CheckBox reverseSecond = find("#reverseSecond");
        ValidableTextField firstStep = find("#alternateMixFirstStep");
        ValidableTextField secondStep = find("#alternateMixSecondStep");
        Map<String, String> data = new HashMap<>();
        data.put("firstStep", "4");
        data.put("secondStep", "3");
        data.put("reverseFirst", Boolean.TRUE.toString());
        data.put("reverseSecond", Boolean.FALSE.toString());
        AlternateMixOptionsPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertEquals("4", firstStep.getText());
        assertEquals("3", secondStep.getText());
        assertTrue(reverseFirst.isSelected());
        assertFalse(reverseSecond.isSelected());
    }
}
