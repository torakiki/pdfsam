/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/set/2014
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
package org.pdfsam.merge;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
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
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.outline.OutlinePolicy;
import org.sejda.model.pdf.form.AcroFormPolicy;
import org.sejda.model.toc.ToCPolicy;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class MergeOptionsPaneTest extends ApplicationTest {

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();

    private MergeParametersBuilder builder;
    private Consumer<String> onError;
    private MergeOptionsPane victim;

    @Before
    public void setUp() {
        builder = mock(MergeParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    public void start(Stage stage) {
        victim = new MergeOptionsPane();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validSteps() {
        clickOn("#blankIfOddCheck");
        victim.apply(builder, onError);
        verify(builder).outlinePolicy(eq(OutlinePolicy.RETAIN));
        verify(builder).blankPageIfOdd(true);
        verify(builder).footer(false);
        verify(builder).normalize(false);
        verify(builder).acroFormsPolicy(AcroFormPolicy.MERGE);
        verify(builder).tocPolicy(ToCPolicy.NONE);
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void onSaveWorkspace() {
        clickOn("#blankIfOddCheck");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals(OutlinePolicy.RETAIN.toString(), data.get("outline"));
        assertEquals(Boolean.TRUE.toString(), data.get("blankIfOdd"));
        assertEquals(Boolean.FALSE.toString(), data.get("footer"));
        assertEquals(Boolean.FALSE.toString(), data.get("normalize"));
        assertEquals(AcroFormPolicy.MERGE.toString(), data.get("acroForms"));
        assertEquals(ToCPolicy.NONE.toString(), data.get("toc"));
    }

    @Test
    public void restoreStateFrom() {
        ComboBox<KeyStringValueItem<OutlinePolicy>> outline = lookup("#outlineCombo").queryComboBox();
        ComboBox<KeyStringValueItem<AcroFormPolicy>> forms = lookup("#acroFormsCombo").queryComboBox();
        ComboBox<KeyStringValueItem<AcroFormPolicy>> toc = lookup("#tocCombo").queryComboBox();
        CheckBox blankIfOdd = lookup("#blankIfOddCheck").queryAs(CheckBox.class);
        CheckBox footer = lookup("#footerCheck").queryAs(CheckBox.class);
        CheckBox normalize = lookup("#normalizeCheck").queryAs(CheckBox.class);
        Map<String, String> data = new HashMap<>();
        data.put("outline", OutlinePolicy.ONE_ENTRY_EACH_DOC.toString());
        data.put("acroForms", AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS.toString());
        data.put("blankIfOdd", Boolean.FALSE.toString());
        data.put("footer", Boolean.TRUE.toString());
        data.put("normalize", Boolean.TRUE.toString());
        data.put("toc", ToCPolicy.DOC_TITLES.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(OutlinePolicy.ONE_ENTRY_EACH_DOC, outline.getSelectionModel().getSelectedItem().getKey());
        assertEquals(AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS,
                forms.getSelectionModel().getSelectedItem().getKey());
        assertEquals(ToCPolicy.DOC_TITLES, toc.getSelectionModel().getSelectedItem().getKey());
        assertFalse(blankIfOdd.isSelected());
        assertTrue(footer.isSelected());
        assertTrue(normalize.isSelected());
    }

    @Test
    public void reset() {
        ComboBox<KeyStringValueItem<OutlinePolicy>> outline = lookup("#outlineCombo").queryComboBox();
        ComboBox<KeyStringValueItem<AcroFormPolicy>> forms = lookup("#acroFormsCombo").queryComboBox();
        ComboBox<KeyStringValueItem<ToCPolicy>> toc = lookup("#tocCombo").queryComboBox();
        CheckBox blankIfOdd = lookup("#blankIfOddCheck").queryAs(CheckBox.class);
        CheckBox footer = lookup("#footerCheck").queryAs(CheckBox.class);
        CheckBox normalize = lookup("#normalizeCheck").queryAs(CheckBox.class);
        Map<String, String> data = new HashMap<>();
        data.put("outline", OutlinePolicy.ONE_ENTRY_EACH_DOC.toString());
        data.put("acroForms", AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS.toString());
        data.put("blankIfOdd", Boolean.TRUE.toString());
        data.put("footer", Boolean.TRUE.toString());
        data.put("normalize", Boolean.TRUE.toString());
        data.put("toc", ToCPolicy.DOC_TITLES.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(OutlinePolicy.ONE_ENTRY_EACH_DOC, outline.getSelectionModel().getSelectedItem().getKey());
        assertEquals(AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS,
                forms.getSelectionModel().getSelectedItem().getKey());
        assertEquals(ToCPolicy.DOC_TITLES, toc.getSelectionModel().getSelectedItem().getKey());
        assertTrue(blankIfOdd.isSelected());
        assertTrue(footer.isSelected());
        assertTrue(normalize.isSelected());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals(OutlinePolicy.RETAIN, outline.getSelectionModel().getSelectedItem().getKey());
        assertEquals(AcroFormPolicy.MERGE, forms.getSelectionModel().getSelectedItem().getKey());
        assertEquals(ToCPolicy.NONE, toc.getSelectionModel().getSelectedItem().getKey());
        assertFalse(blankIfOdd.isSelected());
        assertFalse(footer.isSelected());
        assertFalse(normalize.isSelected());
    }
}
