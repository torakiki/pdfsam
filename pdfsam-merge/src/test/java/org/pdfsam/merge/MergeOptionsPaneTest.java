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
package org.pdfsam.merge;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
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
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.outline.OutlinePolicy;
import org.sejda.model.pdf.form.AcroFormPolicy;

import javafx.scene.Parent;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class MergeOptionsPaneTest extends GuiTest {

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();

    private MergeParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(MergeParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        return new MergeOptionsPane();
    }

    @Test
    public void validSteps() throws Exception {
        click("#blankIfOddCheck");
        MergeOptionsPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(builder).outlinePolicy(eq(OutlinePolicy.RETAIN));
        verify(builder).blankPageIfOdd(true);
        verify(builder).acroFormsPolicy(AcroFormPolicy.MERGE);
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void onSaveWorkspace() {
        click("#blankIfOddCheck");
        Map<String, String> data = new HashMap<>();
        MergeOptionsPane victim = find(".pdfsam-container");
        victim.saveStateTo(data);
        assertEquals(OutlinePolicy.RETAIN.toString(), data.get("outline"));
        assertEquals(Boolean.TRUE.toString(), data.get("blankIfOdd"));
        assertEquals(AcroFormPolicy.MERGE.toString(), data.get("acroForms"));
    }

    @Test
    public void restoreStateFrom() throws Exception {
        ComboBox<KeyStringValueItem<OutlinePolicy>> outline = find("#outlineCombo");
        ComboBox<KeyStringValueItem<OutlinePolicy>> forms = find("#acroFormsCombo");
        CheckBox blankIfOdd = find("#blankIfOddCheck");
        Map<String, String> data = new HashMap<>();
        data.put("outline", OutlinePolicy.ONE_ENTRY_EACH_DOC.toString());
        data.put("acroForms", AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS.toString());
        data.put("blankIfOdd", Boolean.FALSE.toString());
        MergeOptionsPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertEquals(OutlinePolicy.ONE_ENTRY_EACH_DOC, outline.getSelectionModel().getSelectedItem().getKey());
        assertEquals(AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS,
                forms.getSelectionModel().getSelectedItem().getKey());
        assertFalse(blankIfOdd.isSelected());
    }
}
