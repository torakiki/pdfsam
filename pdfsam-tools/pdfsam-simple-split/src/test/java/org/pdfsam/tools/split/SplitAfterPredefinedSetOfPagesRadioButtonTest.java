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
package org.pdfsam.tools.split;

import javafx.scene.Scene;
import javafx.scene.control.ComboBox;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.core.ConfigurableSystemProperty;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.test.ClearEventStudioExtension;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.optimization.OptimizationPolicy;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.parameter.SimpleSplitParameters;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class SplitAfterPredefinedSetOfPagesRadioButtonTest {

    private Consumer<String> onError;
    private SplitAfterPredefinedSetOfPagesRadioButton victim;
    @TempDir
    private Path folder;
    private FxRobot robot;

    @BeforeEach
    public void setUp() {
        onError = mock(Consumer.class);
    }

    @Start
    public void start(Stage stage) {
        var combo = new ComboBox<ComboItem<PredefinedSetOfPages>>();
        combo.setId("combo");
        combo.getItems().add(new ComboItem<>(PredefinedSetOfPages.ALL_PAGES, "Every page"));
        combo.getItems().add(new ComboItem<>(PredefinedSetOfPages.EVEN_PAGES, "Even pages"));
        combo.getItems().add(new ComboItem<>(PredefinedSetOfPages.ODD_PAGES, "Odd pages"));
        victim = new SplitAfterPredefinedSetOfPagesRadioButton(combo);
        victim.setId("victim");
        Scene scene = new Scene(new HBox(victim, combo));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Tag("NoHeadless")
    public void builder() throws Exception {
        robot.clickOn("#combo").clickOn("Odd pages");
        var file = Files.createTempFile(folder, null, ".pdf").toFile();
        SplitAfterPredefinedSetOfPagesRadioButton.SimpleSplitParametersBuilder builder = victim.getBuilder(onError);
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
            assertThat(params.getPages(6)).containsOnly(1, 3, 5);
            assertThat(params.getPages(6)).doesNotContain(2, 4, 6);
            assertEquals("prefix", params.getOutputPrefix());
            assertEquals(output, params.getOutput());
            assertEquals(source, params.getSourceList().get(0));
            assertEquals(OptimizationPolicy.AUTO, params.getOptimizationPolicy());
            verify(onError, never()).accept(anyString());
        });

    }

    @Test
    public void builderDisabledOptimization() throws Exception {
        robot.clickOn("#combo").clickOn("Odd pages");
        var file = Files.createTempFile(folder, null, ".pdf").toFile();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            SplitAfterPredefinedSetOfPagesRadioButton.SimpleSplitParametersBuilder builder = victim.getBuilder(onError);
            builder.compress(true);
            FileOrDirectoryTaskOutput output = mock(FileOrDirectoryTaskOutput.class);
            builder.output(output);
            builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
            builder.prefix("prefix");
            PdfFileSource source = PdfFileSource.newInstanceNoPassword(file);
            builder.source(source);
            builder.version(PdfVersion.VERSION_1_7);
            System.setProperty(ConfigurableSystemProperty.PDFSAM_DISABLE_SPLIT_OPTIMIZATION, Boolean.TRUE.toString());
            SimpleSplitParameters params = builder.build();
            assertEquals(OptimizationPolicy.NO, params.getOptimizationPolicy());
            verify(onError, never()).accept(anyString());
        });
        System.setProperty(ConfigurableSystemProperty.PDFSAM_DISABLE_SPLIT_OPTIMIZATION, Boolean.FALSE.toString());
    }

    @Test
    @Tag("NoHeadless")
    public void saveStateSelected() {
        robot.clickOn(victim);
        robot.clickOn("#combo").clickOn("Odd pages");
        Map<String, String> data = new HashMap<>();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.saveStateTo(data);
            assertTrue(Boolean.parseBoolean(data.get("splitAfterPredefined")));
            assertEquals(PredefinedSetOfPages.ODD_PAGES.toString(), data.get("splitAfterPredefined.combo"));
        });
    }

    @Test
    public void saveStateNotSelected() {
        Map<String, String> data = new HashMap<>();
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            victim.saveStateTo(data);
            assertFalse(Boolean.parseBoolean(data.get("splitAfterPredefined")));
            assertEquals(PredefinedSetOfPages.ALL_PAGES.toString(), data.get("splitAfterPredefined.combo"));
        });
    }

    @Test
    public void restoreState() {
        ComboBox<ComboItem<PredefinedSetOfPages>> combo = robot.lookup("#combo").queryComboBox();
        Map<String, String> data = new HashMap<>();
        data.put("splitAfterPredefined", Boolean.TRUE.toString());
        data.put("splitAfterPredefined.combo", PredefinedSetOfPages.EVEN_PAGES.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertTrue(victim.isSelected());
        assertEquals(PredefinedSetOfPages.EVEN_PAGES, combo.getSelectionModel().getSelectedItem().key());
    }

    @Test
    @Tag("NoHeadless")
    public void reset() {
        robot.clickOn(victim);
        robot.clickOn("#combo").clickOn("Odd pages");
        ComboBox<ComboItem<PredefinedSetOfPages>> combo = robot.lookup("#combo").queryComboBox();
        assertEquals(PredefinedSetOfPages.ODD_PAGES, combo.getSelectionModel().getSelectedItem().key());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals(PredefinedSetOfPages.ALL_PAGES, combo.getSelectionModel().getSelectedItem().key());
    }
}
