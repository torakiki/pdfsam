/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/lug/2014
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
package org.pdfsam.ui.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * @author Andrea Vacondio
 *
 */
public class SetDestinationRequestTest {
    @Rule
    public TemporaryFolder temp = new TemporaryFolder();

    private File file;

    @Before
    public void setUp() throws IOException {
        file = temp.newFile();
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
        assertEquals(new File(file.getParent(), "PDFsam_module.pdf").getAbsolutePath(), victim.getFootprint()
                .getAbsolutePath());
    }
}
