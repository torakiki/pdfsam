/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.model.ui;

import java.io.File;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Request to set a the destination using the given file as footprint.
 *
 * @author Andrea Vacondio
 */
public record SetDestinationRequest(File footprint, boolean fallback) {

    /**
     * @param footprint
     * @param tool
     *            the module requesting
     * @return a request to set the destination for the task to the given file
     */
    public static SetDestinationRequest requestDestination(File footprint, String tool) {
        requireNotNullArg(footprint, "Footprint file cannot be null");
        return new SetDestinationRequest(new File(footprint.getParent(), String.format("PDFsam_%s.pdf", tool)), false);
    }

    /**
     * @param footprint
     * @param tool
     *            the tool requesting
     * @return a request to set the destination as fallback for the task to the given file
     */
    public static SetDestinationRequest requestFallbackDestination(File footprint, String tool) {
        requireNotNullArg(footprint, "Footprint file cannot be null");
        return new SetDestinationRequest(new File(footprint.getParent(), String.format("PDFsam_%s.pdf", tool)), true);
    }
}
