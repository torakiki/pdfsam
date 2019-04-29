/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/gen/2015
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
package org.pdfsam.split;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.NoHeadless;
import org.pdfsam.split.SplitAfterPredefinedSetOfPagesRadioButton.SimpleSplitParametersBuilder;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.support.params.SplitParametersBuilder;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.optimization.OptimizationPolicy;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.parameter.SimpleSplitParameters;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.control.ComboBox;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class SplitAfterPredefinedSetOfPagesRadioButtonTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private Consumer<String> onError;
    private SplitAfterPredefinedSetOfPagesRadioButton victim;

    @Before
    public void setUp() {
        onError = mock(Consumer.class);
    }

    @Override
    public void start(Stage stage) {
        ComboBox<KeyStringValueItem<PredefinedSetOfPages>> combo = new ComboBox<>();
        combo.setId("combo");
        combo.getItems().add(KeyStringValueItem.keyValue(PredefinedSetOfPages.ALL_PAGES, "Every page"));
        combo.getItems().add(KeyStringValueItem.keyValue(PredefinedSetOfPages.EVEN_PAGES, "Even pages"));
        combo.getItems().add(KeyStringValueItem.keyValue(PredefinedSetOfPages.ODD_PAGES, "Odd pages"));
        victim = new SplitAfterPredefinedSetOfPagesRadioButton(combo);
        victim.setId("victim");
        Scene scene = new Scene(new HBox(victim, combo));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Category(NoHeadless.class)
    public void builder() throws Exception {
        clickOn("#combo").clickOn("Odd pages");
        final File file = folder.newFile("my.pdf");
        SimpleSplitParametersBuilder builder = victim.getBuilder(onError);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            builder.compress(true);
            FileOrDirectoryTaskOutput output = mock(FileOrDirectoryTaskOutput.class);
            builder.output(output);
            builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
            builder.prefix("prefix");
            PdfFileSource source = PdfFileSource.newInstanceNoPassword(file);
            builder.source(source);
            builder.version(PdfVersion.VERSION_1_7);
            SimpleSplitParameters params = builder.build();
            assertTrue(params.isCompress());
            assertEquals(ExistingOutputPolicy.OVERWRITE, params.getExistingOutputPolicy());
            assertEquals(PdfVersion.VERSION_1_7, params.getVersion());
            assertThat(params.getPages(6), contains(1, 3, 5));
            assertThat(params.getPages(6), not(contains(2, 4, 6)));
            assertEquals("prefix", params.getOutputPrefix());
            assertEquals(output, params.getOutput());
            assertEquals(source, params.getSourceList().get(0));
            assertEquals(OptimizationPolicy.AUTO, params.getOptimizationPolicy());
            verify(onError, never()).accept(anyString());
        });

    }

    @Test
    public void builderDisabledOptimization() throws Exception {
        clickOn("#combo").clickOn("Odd pages");
        final File file = folder.newFile("my.pdf");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            SimpleSplitParametersBuilder builder = victim.getBuilder(onError);
            builder.compress(true);
            FileOrDirectoryTaskOutput output = mock(FileOrDirectoryTaskOutput.class);
            builder.output(output);
            builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
            builder.prefix("prefix");
            PdfFileSource source = PdfFileSource.newInstanceNoPassword(file);
            builder.source(source);
            builder.version(PdfVersion.VERSION_1_7);
            System.setProperty(SplitParametersBuilder.PDFSAM_DISABLE_SPLIT_OPTIMIZATION, Boolean.TRUE.toString());
            SimpleSplitParameters params = builder.build();
            assertEquals(OptimizationPolicy.NO, params.getOptimizationPolicy());
            verify(onError, never()).accept(anyString());
        });
        System.setProperty(SplitParametersBuilder.PDFSAM_DISABLE_SPLIT_OPTIMIZATION, Boolean.FALSE.toString());
    }

    @Test
    @Category(NoHeadless.class)
    public void saveStateSelected() {
        clickOn(victim);
        clickOn("#combo").clickOn("Odd pages");
        Map<String, String> data = new HashMap<>();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.saveStateTo(data);
            assertTrue(Boolean.valueOf(data.get("splitAfterPredefined")));
            assertEquals(PredefinedSetOfPages.ODD_PAGES.toString(), data.get("splitAfterPredefined.combo"));
        });
    }

    @Test
    public void saveStateNotSelected() {
        Map<String, String> data = new HashMap<>();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.saveStateTo(data);
            assertFalse(Boolean.valueOf(data.get("splitAfterPredefined")));
            assertEquals(PredefinedSetOfPages.ALL_PAGES.toString(), data.get("splitAfterPredefined.combo"));
        });
    }

    @Test
    public void restoreState() {
        ComboBox<KeyStringValueItem<PredefinedSetOfPages>> combo = lookup("#combo").queryComboBox();
        Map<String, String> data = new HashMap<>();
        data.put("splitAfterPredefined", Boolean.TRUE.toString());
        data.put("splitAfterPredefined.combo", PredefinedSetOfPages.EVEN_PAGES.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertTrue(victim.isSelected());
        assertEquals(PredefinedSetOfPages.EVEN_PAGES, combo.getSelectionModel().getSelectedItem().getKey());
    }

    @Test
    @Category(NoHeadless.class)
    public void reset() {
        clickOn(victim);
        clickOn("#combo").clickOn("Odd pages");
        ComboBox<KeyStringValueItem<PredefinedSetOfPages>> combo = lookup("#combo").queryComboBox();
        assertEquals(PredefinedSetOfPages.ODD_PAGES, combo.getSelectionModel().getSelectedItem().getKey());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals(PredefinedSetOfPages.ALL_PAGES, combo.getSelectionModel().getSelectedItem().getKey());
    }
}
