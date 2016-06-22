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
package org.pdfsam.ui.selection.multiple;

import static org.apache.commons.lang3.StringUtils.defaultString;

import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.support.params.ConversionUtils;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.pdf.page.PageRange;

import javafx.beans.property.SimpleStringProperty;

/**
 * Model for a row of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public final class SelectionTableRowData {

    private PdfDocumentDescriptor descriptor;
    private SimpleStringProperty pageSelection = new SimpleStringProperty(StringUtils.EMPTY);

    public SelectionTableRowData(PdfDocumentDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    public SelectionTableRowData(PdfDocumentDescriptor descriptor, String pageSelection) {
        this(descriptor);
        this.pageSelection.set(defaultString(pageSelection));
    }

    public String getPageSelection() {
        return pageSelection.get();
    }

    public void setPageSelection(String pageSelection) {
        this.pageSelection.set(defaultString(pageSelection));
    }

    public SimpleStringProperty pageSelectionProperty() {
        return pageSelection;
    }

    public SelectionTableRowData duplicate() {
        descriptor.retain();
        return new SelectionTableRowData(descriptor, pageSelection.get());
    }

    public PdfDocumentDescriptor descriptor() {
        return descriptor;
    }

    public void invalidate() {
        descriptor.release();
    }

    /**
     * @return the {@link PageRange} selection set if any, an empty set otherwise.
     */
    public Set<PageRange> toPageRangeSet() throws ConversionException {
        return ConversionUtils.toPageRangeSet(pageSelection.get());
    }

}
