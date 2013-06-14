/*
 * Created on 14/giu/2013
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.pdf;

import java.io.File;

import org.junit.Test;
import org.pdfsam.gui.event.EventNamespace;

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
        BasePdfLoadEvent victim = new BasePdfLoadEvent(EventNamespace.NULL);
        assertNotNull(victim.getDocuments());
        assertEquals(0, victim.getDocuments().size());
        victim.add(descriptor);
        assertEquals(1, victim.getDocuments().size());
    }

}
