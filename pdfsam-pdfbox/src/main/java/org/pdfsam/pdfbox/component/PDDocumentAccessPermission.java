/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mar/2015
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

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.encryption.AccessPermission;
import org.sejda.core.Sejda;
import org.sejda.model.exception.TaskPermissionsException;
import org.sejda.model.pdf.encryption.PdfAccessPermission;

/**
 * Wrapper around {@link AccessPermission} providing convenient methods to check permissions.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PDDocumentAccessPermission {

    private AccessPermission permissions;

    PDDocumentAccessPermission(PDDocument document) {
        this.permissions = document.getCurrentAccessPermission();
    }

    /**
     * Ensures that owner permissions are available.
     * 
     * @throws TaskPermissionsException
     *             if not owner permissions are granted.
     */
    public void ensureOwnerPermissions() throws TaskPermissionsException {
        if (!Boolean.getBoolean(Sejda.UNETHICAL_READ_PROPERTY_NAME) && !permissions.isOwnerPermission()) {
            throw new TaskPermissionsException("Owner permission is required.");
        }
    }

    /**
     * Ensures that the required permission is granted
     * 
     * @param required
     * @throws TaskPermissionsException
     *             if not granted.
     */
    public void ensurePermission(PdfAccessPermission required) throws TaskPermissionsException {
        if (!Boolean.getBoolean(Sejda.UNETHICAL_READ_PROPERTY_NAME)
                && !ForwardingPdfAccessPermission.valueFromPdfAccessPermission(required).isAllowed(permissions)) {
            throw new TaskPermissionsException(String.format("Permission %s is not granted.", required));
        }
    }

    /**
     * enum representing a mapping between a {@link PdfAccessPermission} and it's corresponding method on an {@link AccessPermission} instance.
     * 
     * @author Andrea Vacondio
     * 
     */
    private enum ForwardingPdfAccessPermission {
        MODIFY(PdfAccessPermission.MODIFY) {
            @Override
            boolean isAllowed(AccessPermission permissions) {
                return permissions.canModify();
            }
        },
        ASSEMBLE(PdfAccessPermission.ASSEMBLE) {
            @Override
            boolean isAllowed(AccessPermission permissions) {
                return permissions.canAssembleDocument();
            }
        },
        COPY_AND_EXTRACT(PdfAccessPermission.COPY_AND_EXTRACT) {
            @Override
            boolean isAllowed(AccessPermission permissions) {
                return permissions.canExtractContent();
            }
        },
        DEGRADATED_PRINT(PdfAccessPermission.DEGRADATED_PRINT) {
            @Override
            boolean isAllowed(AccessPermission permissions) {
                return permissions.canPrintDegraded();
            }
        },
        EXTRACTION_FOR_DISABLES(PdfAccessPermission.EXTRACTION_FOR_DISABLES) {
            @Override
            boolean isAllowed(AccessPermission permissions) {
                return permissions.canExtractForAccessibility();
            }
        },
        FILL_FORMS(PdfAccessPermission.FILL_FORMS) {
            @Override
            boolean isAllowed(AccessPermission permissions) {
                return permissions.canFillInForm();
            }
        },
        PRINT(PdfAccessPermission.PRINT) {
            @Override
            boolean isAllowed(AccessPermission permissions) {
                return permissions.canPrint();
            }
        },
        ANNOTATION(PdfAccessPermission.ANNOTATION) {
            @Override
            boolean isAllowed(AccessPermission permissions) {
                return permissions.canModifyAnnotations();
            }
        };

        private PdfAccessPermission permission;

        private ForwardingPdfAccessPermission(PdfAccessPermission permission) {
            this.permission = permission;
        }

        abstract boolean isAllowed(AccessPermission permissions);

        static ForwardingPdfAccessPermission valueFromPdfAccessPermission(PdfAccessPermission accessPermission) {
            for (ForwardingPdfAccessPermission current : ForwardingPdfAccessPermission.values()) {
                if (current.permission == accessPermission) {
                    return current;
                }
            }
            throw new IllegalArgumentException(String.format("No Forwarding access permission found for %s",
                    accessPermission));
        }
    }
}
