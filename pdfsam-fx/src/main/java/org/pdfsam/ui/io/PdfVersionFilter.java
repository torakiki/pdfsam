/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/nov/2013
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
package org.pdfsam.ui.io;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javafx.beans.property.ReadOnlyIntegerProperty;
import javafx.beans.property.ReadOnlyIntegerWrapper;

/**
 * Component holding pdf version filter data used filter out pdf versions that don't meet the minimum version number required.
 * 
 * @author Andrea Vacondio
 * 
 */
class PdfVersionFilter {
    private Set<Integer> filters = new HashSet<>();
    private ReadOnlyIntegerWrapper required = new ReadOnlyIntegerWrapper(Integer.MIN_VALUE);

    public void addFilter(Integer version) {
        // the filter is not already there
        if (filters.add(version)) {
            // it's higher then what was there
            if (version > required.get()) {
                required.set(version);
            }
        }
    }

    public void removeFilter(Integer version) {
        // the filter was there
        if (filters.remove(version)) {
            // I removed the highest
            if (version == required.get()) {
                required.set(Collections.max(filters));
            }
        }
    }

    ReadOnlyIntegerProperty requiredProperty() {
        return required.getReadOnlyProperty();
    }

}
