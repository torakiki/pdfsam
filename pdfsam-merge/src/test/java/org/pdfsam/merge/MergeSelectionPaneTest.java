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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.selection.multiple.SelectionTableRowData;
import org.sejda.conversion.exception.ConversionException;

/**
 * @author Andrea Vacondio
 *
 */
public class MergeSelectionPaneTest {
    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clear = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    private MergeSelectionPane victim;
    private MergeParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(MergeParametersBuilder.class);
        onError = mock(Consumer.class);
        victim = new MergeSelectionPane(MODULE);
    }

    @Test
    public void empty() {
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).addInput(any());
    }

    @Test
    public void notEmpty() throws Exception {
        populate();
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).addInput(any());
    }

    @Test
    public void converstionException() throws Exception {
        populate();
        doThrow(new ConversionException("message")).when(builder).addInput(any());
        victim.apply(builder, onError);
        verify(builder).addInput(any());
        verify(onError).accept(eq("message"));
    }

    private void populate() throws Exception {
        File file = folder.newFile("temp.pdf");
        PdfLoadRequestEvent<SelectionTableRowData> loadEvent = new PdfLoadRequestEvent<>(MODULE);
        loadEvent.add(new SelectionTableRowData(file));
        eventStudio().broadcast(loadEvent, MODULE);
    }
}
