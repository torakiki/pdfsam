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

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.sejda.conversion.AdapterUtils.splitAndTrim;

import java.io.File;
import java.util.Collections;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.sejda.common.collection.NullSafeSet;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.pdf.page.PageRange;

/**
 * Model for a row of the selection table
 * 
 * @author Andrea Vacondio
 * 
 */
public final class SelectionTableRowData extends PdfDocumentDescriptor {

    public SelectionTableRowData(File file, String password) {
        super(file, password);
    }

    public SelectionTableRowData(File file) {
        super(file, null);
    }

    private String pageSelection = StringUtils.EMPTY;

    public String getPageSelection() {
        return pageSelection;
    }

    public void setPageSelection(String pageSelection) {
        this.pageSelection = StringUtils.defaultString(pageSelection);
    }

    /**
     * @return the {@link PageRange} selection set if any, an empty set otherwise.
     */
    public Set<PageRange> toPageRangeSet() throws ConversionException {
        if (isNotBlank(pageSelection)) {
            Set<PageRange> pageRangeSet = new NullSafeSet<>();
            String[] tokens = splitAndTrim(pageSelection, ",");
            for (String current : tokens) {
                PageRange range = toPageRange(current);
                if (range.getEnd() < range.getStart()) {
                    throw new ConversionException(DefaultI18nContext.getInstance().i18n("Invalid range: {0}.",
                            range.toString()));
                }
                pageRangeSet.add(range);
            }
            return pageRangeSet;
        }
        return Collections.emptySet();
    }

    private PageRange toPageRange(String value) throws ConversionException {
        String[] limits = splitAndTrim(value, "-");
        if (limits.length > 2) {
            throw new ConversionException(DefaultI18nContext.getInstance().i18n(
                    "Ambiguous page range definition: {0}. Use following formats: [n] or [n1-n2] or [-n] or [n-]",
                    value));
        }
        if (limits.length == 1) {
            int limitNumber = parsePageNumber(limits[0]);
            if (value.endsWith("-")) {
                return new PageRange(limitNumber);
            }
            if (value.startsWith("-")) {
                return new PageRange(1, limitNumber);
            }
            return new PageRange(limitNumber, limitNumber);
        }
        return new PageRange(parsePageNumber(limits[0]), parsePageNumber(limits[1]));
    }

    private int parsePageNumber(String value) throws ConversionException {
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException nfe) {
            throw new ConversionException(DefaultI18nContext.getInstance().i18n("Invalid number: {0}.", value));
        }
    }
}
