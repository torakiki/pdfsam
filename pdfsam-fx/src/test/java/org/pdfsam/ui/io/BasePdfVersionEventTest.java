/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/lug/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.sejda.model.pdf.PdfVersion;

/**
 * @author Andrea Vacondio
 *
 */
public class BasePdfVersionEventTest {

    @Test
    public void nullArg() {
        BasePdfVersionEvent victim = new BasePdfVersionEvent(null);
        assertFalse(victim.hasPdfVersion());
    }

    @Test
    public void notNullArg() {
        BasePdfVersionEvent victim = new BasePdfVersionEvent(PdfVersion.VERSION_1_3);
        assertTrue(victim.hasPdfVersion());
        assertEquals(PdfVersion.VERSION_1_3, victim.getPdfVersion());
    }
}
