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
package org.pdfsam.ui.selection.multiple;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;

import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.NoHeadless;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.matcher.control.TableViewMatchers;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class SelectionTableWithoutMoveTest extends ApplicationTest {
    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Override
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

    @After
    public void tearDown() {
        type(KeyCode.ESCAPE);
    }

    @Test
    @Category(NoHeadless.class)
    public void moveDownIsMissing() {
        rightClickOn(TableViewMatchers.hasTableCell("temp.pdf"));
        assertFalse(lookup(DefaultI18nContext.getInstance().i18n("Move Down")).tryQuery().isPresent());
        assertTrue(lookup(DefaultI18nContext.getInstance().i18n("Set destination")).tryQuery().isPresent());
    }

    @Test
    @Category(NoHeadless.class)
    public void moveUpIsMissing() {
        rightClickOn(TableViewMatchers.hasTableCell("temp.pdf"));
        assertFalse(lookup(DefaultI18nContext.getInstance().i18n("Move Up")).tryQuery().isPresent());
        assertTrue(lookup(DefaultI18nContext.getInstance().i18n("Set destination")).tryQuery().isPresent());
    }

    @Test
    @Category(NoHeadless.class)
    public void moveBottomIsMissing() {
        rightClickOn(TableViewMatchers.hasTableCell("temp.pdf"));
        assertFalse(lookup(DefaultI18nContext.getInstance().i18n("Move to Bottom")).tryQuery().isPresent());
        assertTrue(lookup(DefaultI18nContext.getInstance().i18n("Set destination")).tryQuery().isPresent());
    }

    @Test
    @Category(NoHeadless.class)
    public void moveTopIsMissing() {
        rightClickOn(TableViewMatchers.hasTableCell("temp.pdf"));
        assertFalse(lookup(DefaultI18nContext.getInstance().i18n("Move to Top")).tryQuery().isPresent());
        assertTrue(lookup(DefaultI18nContext.getInstance().i18n("Set destination")).tryQuery().isPresent());

    }

    private PdfDocumentDescriptor populate() throws Exception {
        File file = folder.newFile("temp.pdf");
        PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(MODULE);
        PdfDocumentDescriptor ret = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        loadEvent.add(ret);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> eventStudio().broadcast(loadEvent, MODULE));
        return ret;
    }
}
