/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/giu/2013
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
package org.pdfsam.pdf;

import java.io.File;

import org.junit.Test;
import org.pdfsam.gui.event.String;

import static org.mockito.Mockito.mock;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * @author Andrea Vacondio
 * 
 */
public class BasePdfLoadEventTest {

    @Test
    public void add() {
        File file = mock(File.class);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        BasePdfLoadEvent victim = new BasePdfLoadEvent(String.NULL);
        assertNotNull(victim.getDocuments());
        assertEquals(0, victim.getDocuments().size());
        victim.add(descriptor);
        assertEquals(1, victim.getDocuments().size());
    }

}
