/*
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.selection;

import javafx.scene.layout.Region;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;

import static org.sejda.commons.util.RequireUtils.requireArg;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Request to show a password field popup to ask the user to input the document password
 *
 * @author Andrea Vacondio
 */
public record ShowPasswordFieldPopupRequest(Region requestingNode, PdfDocumentDescriptor... pdfDescriptors) {

    public ShowPasswordFieldPopupRequest {
        requireArg(pdfDescriptors != null && pdfDescriptors.length > 0, "Cannot show password field popup for no document");
        requireNotNullArg(requestingNode, "Cannot show password field popup for a null node");
    }

}
