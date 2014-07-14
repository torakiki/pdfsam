/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/lug/2014
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
package org.pdfsam.ui.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;

import org.junit.Before;
import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class SetDestinationRequestTest {

    private static final String PARENT = "/parent/path";
    private File file;

    @Before
    public void setUp() {
        file = mock(File.class);
        when(file.getParent()).thenReturn(PARENT);
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArg() {
        SetDestinationRequest.requestDestination(null, "module");
    }

    @Test
    public void fallback() {
        assertTrue(SetDestinationRequest.requestFallbackDestination(file, "module").isFallback());
        assertFalse(SetDestinationRequest.requestDestination(file, "module").isFallback());
    }

    @Test
    public void footprint() {
        SetDestinationRequest victim = SetDestinationRequest.requestFallbackDestination(file, "module");
        assertEquals("/parent/path/PDFsam_module.pdf", victim.getFootprint().getAbsolutePath());
    }
}
