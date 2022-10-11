/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/giu/2013
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
package org.pdfsam.ui.components.selection.multiple;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.core.support.params.ConversionUtils;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.pdf.page.PageRange;

import java.util.Set;

/**
 * Model for a row of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public final class SelectionTableRowData {

    private final PdfDocumentDescriptor descriptor;

    public final SimpleStringProperty pageSelection = new SimpleStringProperty(StringUtils.EMPTY);
    public final SimpleStringProperty pace = new SimpleStringProperty("1");
    public final SimpleBooleanProperty reverse = new SimpleBooleanProperty(false);

    public SelectionTableRowData(PdfDocumentDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    public SelectionTableRowData duplicate() {
        descriptor.retain();
        SelectionTableRowData dupe = new SelectionTableRowData(descriptor);
        dupe.pageSelection.set(pageSelection.get());
        dupe.reverse.set(reverse.get());
        dupe.pace.set(pace.get());
        return dupe;
    }

    public PdfDocumentDescriptor descriptor() {
        return descriptor;
    }

    /**
     * Signals that this row data is not valid/needed anymore
     */
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
