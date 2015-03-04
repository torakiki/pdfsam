/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mar/2015
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
package org.pdfsam.pdfbox.component;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.encryption.AccessPermission;
import org.junit.Before;
import org.junit.Test;
import org.sejda.core.Sejda;
import org.sejda.model.exception.TaskPermissionsException;
import org.sejda.model.pdf.encryption.PdfAccessPermission;

/**
 * @author Andrea Vacondio
 * 
 */
public class PDDocumentAccessPermissionTest {

    private PDDocumentAccessPermission victim;
    private AccessPermission permission;

    @Before
    public void setUp() {
        PDDocument document = mock(PDDocument.class);
        permission = mock(AccessPermission.class);
        when(document.getCurrentAccessPermission()).thenReturn(permission);
        victim = new PDDocumentAccessPermission(document);
    }

    @Test(expected = TaskPermissionsException.class)
    public void testNotOwner() throws TaskPermissionsException {
        when(permission.isOwnerPermission()).thenReturn(Boolean.FALSE);
        victim.ensureOwnerPermissions();
    }

    @Test
    public void testOwner() throws TaskPermissionsException {
        when(permission.isOwnerPermission()).thenReturn(Boolean.TRUE);
        victim.ensureOwnerPermissions();
    }

    @Test
    public void testOwnerUnethical() throws TaskPermissionsException {
        when(permission.isOwnerPermission()).thenReturn(Boolean.FALSE);
        System.setProperty(Sejda.UNETHICAL_READ_PROPERTY_NAME, "true");
        victim.ensureOwnerPermissions();
        System.setProperty(Sejda.UNETHICAL_READ_PROPERTY_NAME, "false");
    }

    @Test(expected = TaskPermissionsException.class)
    public void testNotPrint() throws TaskPermissionsException {
        when(permission.canPrint()).thenReturn(Boolean.FALSE);
        victim.ensurePermission(PdfAccessPermission.PRINT);
    }

    @Test
    public void testPrint() throws TaskPermissionsException {
        when(permission.canPrint()).thenReturn(Boolean.TRUE);
        victim.ensurePermission(PdfAccessPermission.PRINT);
    }

    @Test
    public void testPrintUnethical() throws TaskPermissionsException {
        System.setProperty(Sejda.UNETHICAL_READ_PROPERTY_NAME, "true");
        when(permission.canPrint()).thenReturn(Boolean.FALSE);
        victim.ensurePermission(PdfAccessPermission.PRINT);
        System.setProperty(Sejda.UNETHICAL_READ_PROPERTY_NAME, "false");
    }

    @Test(expected = TaskPermissionsException.class)
    public void testNotDegradedPrint() throws TaskPermissionsException {
        when(permission.canPrintDegraded()).thenReturn(Boolean.FALSE);
        victim.ensurePermission(PdfAccessPermission.DEGRADATED_PRINT);
    }

    @Test
    public void testDegradedPrint() throws TaskPermissionsException {
        when(permission.canPrintDegraded()).thenReturn(Boolean.TRUE);
        victim.ensurePermission(PdfAccessPermission.DEGRADATED_PRINT);
    }

    @Test(expected = TaskPermissionsException.class)
    public void testNotAssemble() throws TaskPermissionsException {
        when(permission.canAssembleDocument()).thenReturn(Boolean.FALSE);
        victim.ensurePermission(PdfAccessPermission.ASSEMBLE);
    }

    @Test
    public void testAssemble() throws TaskPermissionsException {
        when(permission.canAssembleDocument()).thenReturn(Boolean.TRUE);
        victim.ensurePermission(PdfAccessPermission.ASSEMBLE);
    }

    @Test(expected = TaskPermissionsException.class)
    public void testNotAannotation() throws TaskPermissionsException {
        when(permission.canModifyAnnotations()).thenReturn(Boolean.FALSE);
        victim.ensurePermission(PdfAccessPermission.ANNOTATION);
    }

    @Test
    public void testAnnotation() throws TaskPermissionsException {
        when(permission.canModifyAnnotations()).thenReturn(Boolean.TRUE);
        victim.ensurePermission(PdfAccessPermission.ANNOTATION);
    }

    @Test(expected = TaskPermissionsException.class)
    public void testNotCopy() throws TaskPermissionsException {
        when(permission.canExtractContent()).thenReturn(Boolean.FALSE);
        victim.ensurePermission(PdfAccessPermission.COPY_AND_EXTRACT);
    }

    @Test
    public void testCopy() throws TaskPermissionsException {
        when(permission.canExtractContent()).thenReturn(Boolean.TRUE);
        victim.ensurePermission(PdfAccessPermission.COPY_AND_EXTRACT);
    }

    @Test(expected = TaskPermissionsException.class)
    public void testNotExtract() throws TaskPermissionsException {
        when(permission.canExtractForAccessibility()).thenReturn(Boolean.FALSE);
        victim.ensurePermission(PdfAccessPermission.EXTRACTION_FOR_DISABLES);
    }

    @Test
    public void testExtract() throws TaskPermissionsException {
        when(permission.canExtractForAccessibility()).thenReturn(Boolean.TRUE);
        victim.ensurePermission(PdfAccessPermission.EXTRACTION_FOR_DISABLES);
    }

    @Test(expected = TaskPermissionsException.class)
    public void testNotFillForm() throws TaskPermissionsException {
        when(permission.canFillInForm()).thenReturn(Boolean.FALSE);
        victim.ensurePermission(PdfAccessPermission.FILL_FORMS);
    }

    @Test
    public void testFillForm() throws TaskPermissionsException {
        when(permission.canFillInForm()).thenReturn(Boolean.TRUE);
        victim.ensurePermission(PdfAccessPermission.FILL_FORMS);
    }

    @Test(expected = TaskPermissionsException.class)
    public void testNotModify() throws TaskPermissionsException {
        when(permission.canModify()).thenReturn(Boolean.FALSE);
        victim.ensurePermission(PdfAccessPermission.MODIFY);
    }

    @Test
    public void testModify() throws TaskPermissionsException {
        when(permission.canModify()).thenReturn(Boolean.TRUE);
        victim.ensurePermission(PdfAccessPermission.MODIFY);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNullPermission() throws TaskPermissionsException {
        victim.ensurePermission(null);
    }
}
