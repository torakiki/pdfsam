/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
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
package org.pdfsam.rotate;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;

import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class RotateSelectionPaneTest extends GuiTest {
    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private RotateParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(RotateParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        RotateSelectionPane victim = new RotateSelectionPane(MODULE);
        victim.setId("victim");
        return victim;
    }

    @Test
    public void empty() throws Exception {
        RotateSelectionPane victim = find("#victim");
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(onError).accept(anyString());
        verify(builder, never()).addSource(any());
    }

    @Test
    public void notEmpty() throws Exception {
        RotateSelectionPane victim = find("#victim");
        populate();
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(onError, never()).accept(anyString());
        verify(builder).addSource(any());
    }

    private void populate() throws Exception {
        File file = folder.newFile("temp.pdf");
        PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(MODULE);
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file));
        FXTestUtils.invokeAndWait(() -> {
            eventStudio().broadcast(loadEvent, MODULE);
        }, 2);
    }
}
