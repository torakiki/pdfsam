/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/giu/2013
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
package org.pdfsam.pdf;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.HashMap;

import org.junit.Before;
import org.junit.Test;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.pdf.PdfVersion;

/**
 * @author Andrea Vacondio
 * 
 */
public class PdfDocumentDescriptorTest {

    private PdfDocumentDescriptor victim;
    private PdfDocumentDescriptor victimNoPwd;
    private File file;

    @Before
    public void setUp() {
        file = mock(File.class);
        when(file.getName()).thenReturn("myName");
        when(file.isFile()).thenReturn(true);
        victim = PdfDocumentDescriptor.newDescriptor(file, "pwd");
        victimNoPwd = PdfDocumentDescriptor.newDescriptorNoPassword(file);
    }

    @Test(expected = IllegalArgumentException.class)
    public void illegal() {
        PdfDocumentDescriptor.newDescriptorNoPassword(null);
    }

    @Test
    public void initialState() {
        assertTrue(victim.hasReferences());
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, victim.loadingStatus().getValue());
        assertEquals("pwd", victim.getPassword());
        assertEquals("myName", victim.getFileName());
        assertNull(victimNoPwd.getPassword());
    }

    @Test
    public void invalidate() {
        victim.retain().retain();
        assertTrue(victim.hasReferences());
        victim.releaseAll();
        assertFalse(victim.hasReferences());
    }

    @Test
    public void retainAndRelease() {
        assertFalse(victim.retain().retain().release());
    }

    @Test
    public void noVersionString() {
        assertEquals("", victim.getVersionString());
    }

    @Test
    public void getVersionString() {
        victim.setVersion(PdfVersion.VERSION_1_5);
        assertFalse(isBlank(victim.getVersionString()));
    }

    @Test
    public void moveValidStatus() {
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, victim.loadingStatus().getValue());
        victim.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        assertEquals(PdfDescriptorLoadingStatus.REQUESTED, victim.loadingStatus().getValue());
    }

    @Test(expected = IllegalStateException.class)
    public void moveInvalidStatus() {
        assertEquals(PdfDescriptorLoadingStatus.INITIAL, victim.loadingStatus().getValue());
        victim.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
    }

    @Test
    public void toPdfSource() {
        PdfFileSource source = victim.toPdfFileSource();
        assertEquals(file, source.getSource());
        assertEquals("pwd", source.getPassword());
    }

    @Test(expected = ConversionException.class)
    public void FailToPdfSource() {
        when(file.isFile()).thenReturn(Boolean.FALSE);
        victim.toPdfFileSource();
    }

    @Test
    public void informationDictionary() {
        HashMap<String, String> values = new HashMap<>();
        values.put("key", "value");
        victim.setInformationDictionary(values);
        assertEquals("value", victim.getInformation("key"));
    }

    @Test
    public void putInformation() {
        victim.putInformation("key", "value");
        assertEquals("value", victim.getInformation("key"));
    }

}
