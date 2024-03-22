/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/set/2014
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
package org.pdfsam.tools.merge;

import javafx.scene.Scene;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.test.ClearEventStudioExtension;
import org.sejda.model.outline.OutlinePolicy;
import org.sejda.model.pdf.form.AcroFormPolicy;
import org.sejda.model.scale.PageNormalizationPolicy;
import org.sejda.model.toc.ToCPolicy;
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
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class MergeOptionsPaneTest {

    private MergeParametersBuilder builder;
    private Consumer<String> onError;
    private MergeOptionsPane victim;
    private FxRobot robot;

    @BeforeEach
    public void setUp() {
        builder = mock(MergeParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Start
    public void start(Stage stage) {
        victim = new MergeOptionsPane();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validSteps() {
        robot.clickOn("#blankIfOddCheck");
        victim.apply(builder, onError);
        verify(builder).outlinePolicy(eq(OutlinePolicy.RETAIN));
        verify(builder).blankPageIfOdd(true);
        verify(builder).footer(false);
        verify(builder).pageNormalizationPolicy(PageNormalizationPolicy.NONE);
        verify(builder).acroFormsPolicy(AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS);
        verify(builder).tocPolicy(ToCPolicy.NONE);
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void onSaveWorkspace() {
        robot.clickOn("#blankIfOddCheck");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals(OutlinePolicy.RETAIN.toString(), data.get("outline"));
        assertEquals(Boolean.TRUE.toString(), data.get("blankIfOdd"));
        assertEquals(Boolean.FALSE.toString(), data.get("footer"));
        assertEquals(PageNormalizationPolicy.NONE.toString(), data.get("pageNormalization"));
        assertEquals(AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS.toString(), data.get("acroForms"));
        assertEquals(ToCPolicy.NONE.toString(), data.get("toc"));
    }

    @Test
    public void restoreStateFrom() {
        ComboBox<ComboItem<OutlinePolicy>> outline = robot.lookup("#outlineCombo").queryComboBox();
        ComboBox<ComboItem<AcroFormPolicy>> forms = robot.lookup("#acroFormsCombo").queryComboBox();
        ComboBox<ComboItem<ToCPolicy>> toc = robot.lookup("#tocCombo").queryComboBox();
        CheckBox blankIfOdd = robot.lookup("#blankIfOddCheck").queryAs(CheckBox.class);
        CheckBox footer = robot.lookup("#footerCheck").queryAs(CheckBox.class);
        ComboBox<ComboItem<PageNormalizationPolicy>> normalize = robot.lookup("#normalizeCheck").queryComboBox();
        Map<String, String> data = new HashMap<>();
        data.put("outline", OutlinePolicy.ONE_ENTRY_EACH_DOC.toString());
        data.put("acroForms", AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS.toString());
        data.put("blankIfOdd", Boolean.FALSE.toString());
        data.put("footer", Boolean.TRUE.toString());
        data.put("pageNormalization", PageNormalizationPolicy.SAME_WIDTH_ORIENTATION_BASED.toString());
        data.put("toc", ToCPolicy.DOC_TITLES.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(OutlinePolicy.ONE_ENTRY_EACH_DOC, outline.getSelectionModel().getSelectedItem().key());
        assertEquals(AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS, forms.getSelectionModel().getSelectedItem().key());
        assertEquals(ToCPolicy.DOC_TITLES, toc.getSelectionModel().getSelectedItem().key());
        assertEquals(PageNormalizationPolicy.SAME_WIDTH_ORIENTATION_BASED,
                normalize.getSelectionModel().getSelectedItem().key());
        assertFalse(blankIfOdd.isSelected());
        assertTrue(footer.isSelected());
    }

    @Test
    public void restoreNormalizationStateBackwardCompatible() {
        ComboBox<ComboItem<PageNormalizationPolicy>> normalize = robot.lookup("#normalizeCheck").queryComboBox();
        Map<String, String> data = new HashMap<>();
        data.put("normalize", Boolean.TRUE.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(PageNormalizationPolicy.SAME_WIDTH_ORIENTATION_BASED,
                normalize.getSelectionModel().getSelectedItem().key());
    }

    @Test
    public void reset() {
        ComboBox<ComboItem<OutlinePolicy>> outline = robot.lookup("#outlineCombo").queryComboBox();
        ComboBox<ComboItem<AcroFormPolicy>> forms = robot.lookup("#acroFormsCombo").queryComboBox();
        ComboBox<ComboItem<ToCPolicy>> toc = robot.lookup("#tocCombo").queryComboBox();
        CheckBox blankIfOdd = robot.lookup("#blankIfOddCheck").queryAs(CheckBox.class);
        CheckBox footer = robot.lookup("#footerCheck").queryAs(CheckBox.class);
        ComboBox<ComboItem<PageNormalizationPolicy>> normalize = robot.lookup("#normalizeCheck").queryComboBox();
        Map<String, String> data = new HashMap<>();
        data.put("outline", OutlinePolicy.ONE_ENTRY_EACH_DOC.toString());
        data.put("acroForms", AcroFormPolicy.FLATTEN.toString());
        data.put("blankIfOdd", Boolean.TRUE.toString());
        data.put("footer", Boolean.TRUE.toString());
        data.put("pageNormalization", PageNormalizationPolicy.SAME_WIDTH_ORIENTATION_BASED.toString());
        data.put("toc", ToCPolicy.DOC_TITLES.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(OutlinePolicy.ONE_ENTRY_EACH_DOC, outline.getSelectionModel().getSelectedItem().key());
        assertEquals(AcroFormPolicy.FLATTEN, forms.getSelectionModel().getSelectedItem().key());
        assertEquals(ToCPolicy.DOC_TITLES, toc.getSelectionModel().getSelectedItem().key());
        assertTrue(blankIfOdd.isSelected());
        assertTrue(footer.isSelected());
        assertEquals(PageNormalizationPolicy.SAME_WIDTH_ORIENTATION_BASED,
                normalize.getSelectionModel().getSelectedItem().key());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals(OutlinePolicy.RETAIN, outline.getSelectionModel().getSelectedItem().key());
        assertEquals(AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS, forms.getSelectionModel().getSelectedItem().key());
        assertEquals(ToCPolicy.NONE, toc.getSelectionModel().getSelectedItem().key());
        assertFalse(blankIfOdd.isSelected());
        assertFalse(footer.isSelected());
        assertEquals(PageNormalizationPolicy.NONE, normalize.getSelectionModel().getSelectedItem().key());
    }
}
