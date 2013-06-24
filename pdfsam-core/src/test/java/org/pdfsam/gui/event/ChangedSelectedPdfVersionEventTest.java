/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/feb/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.event;

import org.junit.Test;
import org.sejda.model.pdf.PdfVersion;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author Andrea Vacondio
 * 
 */
public class ChangedSelectedPdfVersionEventTest {

    @Test
    public void testNoVersion() {
        ChangedSelectedPdfVersionEvent victim = new ChangedSelectedPdfVersionEvent(EventNamespace.NULL, null);
        assertFalse(victim.hasPdfVersion());
        ChangedSelectedPdfVersionEvent victim2 = new ChangedSelectedPdfVersionEvent(EventNamespace.NULL,
                PdfVersion.VERSION_1_0);
        assertTrue(victim2.hasPdfVersion());
    }
}
