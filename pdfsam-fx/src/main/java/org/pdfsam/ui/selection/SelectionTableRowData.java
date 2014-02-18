/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/giu/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.pdf.PdfDocumentDescriptor;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Model for a row of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionTableRowData {

    private PdfDocumentDescriptor documentDescriptor;
    private String pageSelection = StringUtils.EMPTY;

    public SelectionTableRowData(PdfDocumentDescriptor documentDescriptor) {
        requireNotNull(documentDescriptor, "Document descriptor cannot be null");
        this.documentDescriptor = documentDescriptor;
    }

    public String getPageSelection() {
        return pageSelection;
    }

    public void setPageSelection(String pageSelection) {
        this.pageSelection = StringUtils.defaultString(pageSelection);
    }

    public PdfDocumentDescriptor getDocumentDescriptor() {
        return documentDescriptor;
    }

}
