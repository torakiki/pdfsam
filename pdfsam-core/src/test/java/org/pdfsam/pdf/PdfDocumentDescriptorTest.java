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
import org.sejda.model.pdf.PdfMetadataKey;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * @author Andrea Vacondio
 * 
 */
public class PdfDocumentDescriptorTest {

    @Test
    public void copy() {
        File file = mock(File.class);
        PdfDocumentDescriptor victim = PdfDocumentDescriptor.newDescriptor(file, "pwd");
        victim.setEncryptionStatus(EncryptionStatus.DECRYPTED_WITH_OWNER_PWD);
        victim.setVersion("XX");
        victim.setPages(2);
        victim.addMedatada(PdfMetadataKey.AUTHOR, "Chuck Norris");
        victim.addMedatada(PdfMetadataKey.SUBJECT, "test");
        PdfDocumentDescriptor copy = PdfDocumentDescriptor.newCopy(victim);
        assertEquals(victim.getUuid(), copy.getUuid());
        assertEquals(victim.getFile(), copy.getFile());
        assertEquals(victim.getPages(), copy.getPages());
        assertEquals(victim.getPassword(), copy.getPassword());
        assertEquals(victim.getVersion(), copy.getVersion());
        assertEquals(victim.getEncryptionStatus(), copy.getEncryptionStatus());
        for (PdfMetadataKey key : PdfMetadataKey.values()) {
            assertEquals(victim.getMedatada(key), copy.getMedatada(key));
        }
    }

    @Test
    public void getName() {
        File file = mock(File.class);
        when(file.getName()).thenReturn("myName");
        PdfDocumentDescriptor victim = PdfDocumentDescriptor.newDescriptor(file, "pwd");
        assertNotNull(victim.getFileName());
        verify(file).getName();
    }
}
