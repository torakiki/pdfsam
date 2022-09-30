/*
 * This file is part of the PDF Split And Merge source code
 * Created on 14 dic 2015
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
package org.pdfsam.ui.components.selection.multiple;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.stage.Stage;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.matcher.control.TableViewMatchers;
import org.testfx.util.WaitForAsyncUtils;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
public class SelectionTableWithoutMoveTest {
    private static final String MODULE = "MODULE";
    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(MODULE);

    @TempDir
    public Path folder;
    private FxRobot robot;

    @Start
    public void start(Stage stage) throws Exception {
        SelectionTable victim = new SelectionTable(MODULE, true, false,
                new SelectionTableColumn<?>[] { new LoadingColumn(MODULE), FileColumn.NAME, LongColumn.SIZE,
                        IntColumn.PAGES, LongColumn.LAST_MODIFIED, new PageRangesColumn() });
        victim.setId("victim");
        populate();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @AfterEach
    public void tearDown() {
        robot.type(KeyCode.ESCAPE);
    }

    @Test
    @Tag("NoHeadless")
    public void moveDownIsMissing() {
        robot.rightClickOn(TableViewMatchers.hasTableCell("temp.pdf"));
        assertFalse(robot.lookup(i18n().tr("Move Down")).tryQuery().isPresent());
        assertTrue(robot.lookup(i18n().tr("Set destination")).tryQuery().isPresent());
    }

    @Test
    @Tag("NoHeadless")
    public void moveUpIsMissing() {
        robot.rightClickOn(TableViewMatchers.hasTableCell("temp.pdf"));
        assertFalse(robot.lookup(i18n().tr("Move Up")).tryQuery().isPresent());
        assertTrue(robot.lookup(i18n().tr("Set destination")).tryQuery().isPresent());
    }

    @Test
    @Tag("NoHeadless")
    public void moveBottomIsMissing() {
        robot.rightClickOn(TableViewMatchers.hasTableCell("temp.pdf"));
        assertFalse(robot.lookup(i18n().tr("Move to Bottom")).tryQuery().isPresent());
        assertTrue(robot.lookup(i18n().tr("Set destination")).tryQuery().isPresent());
    }

    @Test
    @Tag("NoHeadless")
    public void moveTopIsMissing() {
        robot.rightClickOn(TableViewMatchers.hasTableCell("temp.pdf"));
        assertFalse(robot.lookup(i18n().tr("Move to Top")).tryQuery().isPresent());
        assertTrue(robot.lookup(i18n().tr("Set destination")).tryQuery().isPresent());

    }

    private PdfDocumentDescriptor populate() throws Exception {
        var file = Files.createFile(folder.resolve("temp.pdf"));
        PdfLoadRequest loadEvent = new PdfLoadRequest(MODULE);
        PdfDocumentDescriptor ret = PdfDocumentDescriptor.newDescriptorNoPassword(file.toFile());
        loadEvent.add(ret);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> eventStudio().broadcast(loadEvent, MODULE));
        return ret;
    }
}
