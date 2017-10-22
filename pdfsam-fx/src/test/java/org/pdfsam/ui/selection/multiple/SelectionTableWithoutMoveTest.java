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

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;

import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.exceptions.NoNodesFoundException;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;

import javafx.scene.Parent;
import javafx.scene.input.KeyCode;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class SelectionTableWithoutMoveTest extends GuiTest {
    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Override
    protected Parent getRootNode() {
        SelectionTable victim = new SelectionTable(MODULE, true, false,
                new SelectionTableColumn<?>[] { new LoadingColumn(MODULE), FileColumn.NAME, LongColumn.SIZE,
                        IntColumn.PAGES, LongColumn.LAST_MODIFIED, new PageRangesColumn() });
        victim.setId("victim");
        return victim;
    }

    @After
    public void tearDown() {
        type(KeyCode.ESCAPE);
    }

    @Test(expected = NoNodesFoundException.class)
    public void moveDownIsMissing() throws Exception {
        populate();
        rightClick("temp.pdf");
        find(DefaultI18nContext.getInstance().i18n("Move Down"));
    }

    @Test(expected = NoNodesFoundException.class)
    public void moveUpIsMissing() throws Exception {
        populate();
        rightClick("temp.pdf");
        find(DefaultI18nContext.getInstance().i18n("Move Up"));
    }

    @Test(expected = NoNodesFoundException.class)
    public void moveBottomIsMissing() throws Exception {
        populate();
        rightClick("temp.pdf");
        find(DefaultI18nContext.getInstance().i18n("Move to Bottom"));
    }

    @Test(expected = NoNodesFoundException.class)
    public void moveTopIsMissing() throws Exception {
        populate();
        rightClick("temp.pdf");
        find(DefaultI18nContext.getInstance().i18n("Move to Top"));
    }

    private PdfDocumentDescriptor populate() throws Exception {
        File file = folder.newFile("temp.pdf");
        PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(MODULE);
        PdfDocumentDescriptor ret = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        loadEvent.add(ret);
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(loadEvent, MODULE);
        } , 2);
        return ret;
    }
}
