/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
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
package org.pdfsam.ui.selection;

import static org.pdfsam.support.RequireUtils.requireNotNull;

import org.pdfsam.pdf.PdfDocumentDescriptor;

import javafx.scene.layout.Region;

/**
 * Request to show a password field popup to ask the user to input the document password
 * 
 * @author Andrea Vacondio
 *
 */
public class ShowPasswordFieldPopupRequest {
    private PdfDocumentDescriptor pdfDescriptor;
    private Region requestingNode;

    public ShowPasswordFieldPopupRequest(PdfDocumentDescriptor pdfDescriptor, Region requestingNode) {
        requireNotNull(pdfDescriptor, "Cannot show password field popup for a null document");
        requireNotNull(requestingNode, "Cannot show password field popup for a null node");
        this.pdfDescriptor = pdfDescriptor;
        this.requestingNode = requestingNode;
    }

    public PdfDocumentDescriptor getPdfDescriptor() {
        return pdfDescriptor;
    }

    public Region getRequestingNode() {
        return requestingNode;
    }

}
