/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.ui.io;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.pdfsam.context.UserContext;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.commons.SetDestinationRequest;
import org.sejda.model.pdf.PdfVersion;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(MockitoJUnitRunner.class)
public class PdfDestinationPaneTest {

    private static final String MODULE = "MODULE";
    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Mock
    private UserContext userContext;

    private BrowsableDirectoryField destination;
    private PdfDestinationPane victim;

    @Before
    public void setUp() {
        destination = spy(new BrowsableDirectoryField());
        when(userContext.isUseSmartOutput()).thenReturn(Boolean.FALSE);
        victim = new PdfDestinationPane(destination, MODULE, userContext);
    }

    @Test
    public void setDestination() throws IOException {
        File footprint = folder.newFile("test.pdf");
        SetDestinationRequest event = SetDestinationRequest.requestDestination(footprint, MODULE);
        victim.setDestination(event);
        verify(destination).setTextFromFile(event.getFootprint());
    }

    @Test
    public void setFallbackDestination() throws IOException {
        destination.getTextField().setText("");
        when(userContext.isUseSmartOutput()).thenReturn(Boolean.TRUE);
        File footprint = folder.newFile("test.pdf");
        SetDestinationRequest event = SetDestinationRequest.requestFallbackDestination(footprint, MODULE);
        victim.setDestination(event);
        verify(destination).setTextFromFile(event.getFootprint());
    }

    @Test
    public void dontSetFallbackDestinationIfFilled() throws IOException {
        destination.getTextField().setText("ChuckNorris");
        File footprint = folder.newFile("test.pdf");
        SetDestinationRequest event = SetDestinationRequest.requestDestination(footprint, MODULE);
        victim.setDestination(event);
        verify(destination).setTextFromFile(event.getFootprint());
    }

    @Test
    public void reset() {
        destination.getTextField().setText("ChuckNorris");
        victim.overwrite().setSelected(true);
        victim.resetView();
        assertEquals("", destination.getTextField().getText());
        assertFalse(victim.overwrite().isSelected());

    }

    @Test
    public void dontSetFallbackDestinationIfNoSmartOutput() throws IOException {
        File footprint = folder.newFile("test.pdf");
        SetDestinationRequest event = SetDestinationRequest.requestFallbackDestination(footprint, MODULE);
        victim.setDestination(event);
        verify(destination, never()).setTextFromFile(any());
    }

    @Test
    public void saveState() {
        victim.overwrite().setSelected(true);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals(Boolean.TRUE.toString(), data.get("overwrite"));
        assertEquals(Boolean.TRUE.toString(), data.get("compress"));
        assertNull(data.get("discardBookmarks"));
        assertTrue(isNotEmpty(data.get("version")));
    }

    @Test
    public void restoreState() {
        Map<String, String> data = new HashMap<>();
        data.put("overwrite", Boolean.FALSE.toString());
        data.put("compress", Boolean.TRUE.toString());
        data.put("version", PdfVersion.VERSION_1_4.toString());
        victim.restoreStateFrom(data);
        assertFalse(victim.overwrite().isSelected());
    }
}
