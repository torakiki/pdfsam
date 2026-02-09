/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/nov/2025
 * Copyright 2025 by Sober Lemur S.r.l. (info@soberlemur.com).
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
 * Request to set an output directory using the given file as footprint.
 * The directory will be created based on the input file name (without extension).
 *
 * @author Andrea Vacondio
 */
public record SetOutputDirectoryRequest(File directory, boolean fallback) {

    /**
     * @param footprint input file to use for generating the output directory name
     * @return a request to set the output directory based on the footprint file name
     */
    public static SetOutputDirectoryRequest requestOutputDirectory(File footprint) {
        requireNotNullArg(footprint, "Footprint file cannot be null");
        String nameWithoutExtension = getNameWithoutExtension(footprint.getName());
        return new SetOutputDirectoryRequest(new File(footprint.getParent(), nameWithoutExtension), false);
    }

    /**
     * @param footprint input file to use for generating the output directory name
     * @return a request to set the output directory as fallback based on the footprint file name
     */
    public static SetOutputDirectoryRequest requestFallbackOutputDirectory(File footprint) {
        requireNotNullArg(footprint, "Footprint file cannot be null");
        String nameWithoutExtension = getNameWithoutExtension(footprint.getName());
        return new SetOutputDirectoryRequest(new File(footprint.getParent(), nameWithoutExtension), true);
    }

    /**
     * Removes the file extension from a filename
     */
    private static String getNameWithoutExtension(String filename) {
        int lastDotIndex = filename.lastIndexOf('.');
        if (lastDotIndex > 0) {
            return filename.substring(0, lastDotIndex);
        }
        return filename;
    }
}
