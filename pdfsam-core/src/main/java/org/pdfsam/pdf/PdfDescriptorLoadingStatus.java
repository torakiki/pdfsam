/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/mag/2014
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
package org.pdfsam.pdf;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * Possible loading status for a descriptor
 * 
 * @author Andrea Vacondio
 *
 */
public enum PdfDescriptorLoadingStatus {
    LOADED,
    LOADED_WITH_USER_PWD_DECRYPTION,
    ENCRYPTED,
    LOADING,
    REQUESTED,
    WITH_ERRORS;

    static {
        ENCRYPTED.setValidStatus(REQUESTED, WITH_ERRORS);
        LOADING.setValidStatus(LOADED, LOADED_WITH_USER_PWD_DECRYPTION, ENCRYPTED, WITH_ERRORS);
        REQUESTED.setValidStatus(LOADING, WITH_ERRORS);
        WITH_ERRORS.setValidStatus(REQUESTED, WITH_ERRORS);
    }

    private Set<PdfDescriptorLoadingStatus> validNext = new HashSet<>();

    private void setValidStatus(PdfDescriptorLoadingStatus... canMoveTo) {
        Arrays.stream(canMoveTo).parallel().forEach(validNext::add);
    }

    /**
     * @param dest
     * @return true if the {@link PdfDescriptorLoadingStatus} can move to the given destination status
     */
    public boolean canMoveTo(PdfDescriptorLoadingStatus dest) {
        return validNext.contains(dest);
    }

    /**
     * Moves the current status to the destination one if allowed
     * 
     * @param dest
     * @return the destination status
     */
    public PdfDescriptorLoadingStatus moveTo(PdfDescriptorLoadingStatus dest) {
        if (canMoveTo(dest)) {
            return dest;
        }
        throw new IllegalStateException("Cannot move status from " + this + " to " + dest);
    }
}
