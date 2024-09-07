/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22 giu 2016
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
package org.pdfsam.core.support.params;

import org.apache.commons.lang3.StringUtils;
import org.sejda.commons.collection.NullSafeSet;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.pdf.page.PageRange;
import org.sejda.model.pdf.page.PagesSelection;

import java.util.Arrays;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.apache.commons.lang3.StringUtils.split;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.conversion.AdapterUtils.splitAndTrim;

/**
 * @author Andrea Vacondio
 */
public final class ConversionUtils {

    private ConversionUtils() {
        // hide
    }

    /**
     * @return the {@link PageRange} set for the given string, an empty set otherwise.
     */
    public static NullSafeSet<PageRange> toPageRangeSet(String selection) throws ConversionException {
        if (isNotBlank(selection)) {
            var pageRangeSet = new NullSafeSet<PageRange>();
            Arrays.stream(split(selection, ",")).map(StringUtils::strip).map(ConversionUtils::toPageRange)
                    .forEachOrdered(pageRangeSet::add);
            return pageRangeSet;
        }
        return new NullSafeSet<>();
    }

    /**
     * @return the {@link PagesSelection} set for the given string, an empty set otherwise.
     */
    public static NullSafeSet<PagesSelection> toPagesSelectionSet(String selection) throws ConversionException {
        if (isNotBlank(selection)) {
            var pageRangeSet = new NullSafeSet<PagesSelection>();
            Arrays.stream(split(selection, ",")).map(StringUtils::strip).map(ConversionUtils::toPageSelection)
                    .forEachOrdered(pageRangeSet::add);
            return pageRangeSet;
        }
        return new NullSafeSet<>();
    }

    private static PageRange toPageRange(String value) throws ConversionException {
        String[] limits = splitAndTrim(value, "-");
        if (limits.length > 2) {
            throw new ConversionException(i18n().tr(
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
            return PageRange.one(limitNumber);
        }
        var range = new PageRange(parsePageNumber(limits[0]), parsePageNumber(limits[1]));
        if (range.getEnd() < range.getStart()) {
            throw new ConversionException(i18n().tr("Invalid range: {0}.", range.toString()));
        }
        return range;
    }

    private static int parsePageNumber(String value) throws ConversionException {
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException nfe) {
            throw new ConversionException(i18n().tr("Invalid number: {0}.", value));
        }
    }

    private static PagesSelection toPageSelection(String value) {
        if ("last".equals(value)) {
            return PagesSelection.LAST_PAGE;
        }
        return toPageRange(value);
    }
}
