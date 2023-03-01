package org.pdfsam.ui.components.selection.multiple;

import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/12/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@pdfsam.org).
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
@ExtendWith(ApplicationExtension.class)
class MultipleSelectionPaneTest {
    private static final String MODULE = "MODULE";
    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(MODULE);
    @TempDir
    public Path folder;
    private FxRobot robot;
    private MultipleSelectionPane victim;
    private List<PdfDocumentDescriptor> descriptors;

    @BeforeAll
    public static void setUp() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Start
    public void start(Stage stage) throws Exception {
        victim = new MultipleSelectionPane(MODULE, true, true, FileColumn.NAME, LongColumn.SIZE, IntColumn.PAGES,
                new SelectedPagesColumn(), new PageRangesColumn());
        victim.setId("victim");
        victim.showTotalPagesLabel();
        descriptors = populate();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    private List<PdfDocumentDescriptor> populate() throws Exception {
        File file = Files.createFile(folder.resolve("temp.pdf")).toFile();
        File file2 = Files.createFile(folder.resolve("temp3.pdf")).toFile();
        File file3 = Files.createFile(folder.resolve("temp4.pdf")).toFile();
        var loadEvent = new PdfLoadRequest(MODULE);
        var descriptors = List.of(PdfDocumentDescriptor.newDescriptorNoPassword(file),
                PdfDocumentDescriptor.newDescriptorNoPassword(file2),
                PdfDocumentDescriptor.newDescriptorNoPassword(file3));
        descriptors.forEach(loadEvent::add);
        WaitForAsyncUtils.waitForAsyncFx(1000, () -> eventStudio().broadcast(loadEvent, MODULE));
        descriptors.forEach(d -> d.pages(5));
        return descriptors;
    }

    @Test
    @Tag("NoHeadless")
    public void totalIsUpdated() {
        Optional<SelectionTableRowData> item = victim.table().getItems().stream()
                .filter(i -> "temp.pdf".equals(i.descriptor().getFileName())).findFirst();
        assertTrue(item.isPresent());
        item.get().pageSelection.set("2-4");
        robot.rightClickOn("temp.pdf");
        robot.clickOn(i18n().tr("Set as range for all"));
        assertThat(robot.lookup("#total-label").queryAs(Label.class).getText()).contains("9");

    }
}