/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/ott/2014
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
package org.pdfsam.ui.dialog;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.sejda.model.exception.TaskOutputVisitException;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.MergeParameters;
import org.sejda.model.parameter.SimpleSplitParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;

/**
 * @author Andrea Vacondio
 *
 */
public class OverwriteDialogControllerTest {

    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();

    private OverwriteDialogController victim;
    private OverwriteConfirmationDialog dialog;

    @Before
    public void setUp() {
        dialog = mock(OverwriteConfirmationDialog.class);
        victim = new OverwriteDialogController(dialog);
    }

    @Test
    public void isOverwrite() throws TaskOutputVisitException {
        MergeParameters parameters = new MergeParameters();
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.OVERWRITE);
        FileTaskOutput output = mock(FileTaskOutput.class);
        parameters.setOutput(output);
        victim.request(new TaskExecutionRequestEvent("id", parameters));
        verify(output, never()).accept(any());
    }

    @Test
    public void fileDoesntExists() {
        MergeParameters parameters = new MergeParameters();
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.FAIL);
        FileTaskOutput output = mock(FileTaskOutput.class);
        File file = mock(File.class);
        when(file.exists()).thenReturn(Boolean.FALSE);
        when(output.getDestination()).thenReturn(file);
        parameters.setOutput(output);
        victim.request(new TaskExecutionRequestEvent("id", parameters));
        verify(dialog, never()).title(anyString());
        verify(dialog, never()).messageContent(anyString());
        verify(dialog, never()).messageTitle(anyString());
        verify(dialog, never()).response();
    }

    @Test
    public void directoryIsEmpty() {
        SimpleSplitParameters parameters = new SimpleSplitParameters(PredefinedSetOfPages.ALL_PAGES);
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.FAIL);
        DirectoryTaskOutput output = mock(DirectoryTaskOutput.class);
        File file = mock(File.class);
        when(file.listFiles()).thenReturn(new File[0]);
        when(output.getDestination()).thenReturn(file);
        parameters.setOutput(output);
        victim.request(new TaskExecutionRequestEvent("id", parameters));
        verify(dialog, never()).title(anyString());
        verify(dialog, never()).messageContent(anyString());
        verify(dialog, never()).messageTitle(anyString());
        verify(dialog, never()).response();
    }

}
