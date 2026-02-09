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
package org.pdfsam.tools.splitbybookmarks;

import org.sejda.model.parameter.SplitByOutlineLevelParameters;

/**
 * Extension of SplitByOutlineLevelParameters that supports hierarchical directory structure output.
 * When hierarchical mode is enabled, creates directories for parent bookmarks and places split files inside.
 *
 * @author Andrea Vacondio
 */
public class HierarchicalSplitByOutlineLevelParameters extends SplitByOutlineLevelParameters {

    private final boolean hierarchicalOutput;
    private final int overlapPages;
    private final boolean autoDetectOverlap;

    public HierarchicalSplitByOutlineLevelParameters(int level, boolean hierarchicalOutput, int overlapPages, boolean autoDetectOverlap) {
        super(level);
        this.hierarchicalOutput = hierarchicalOutput;
        this.overlapPages = overlapPages;
        this.autoDetectOverlap = autoDetectOverlap;
    }

    public boolean isHierarchicalOutput() {
        return hierarchicalOutput;
    }

    public int getOverlapPages() {
        return overlapPages;
    }

    public boolean isAutoDetectOverlap() {
        return autoDetectOverlap;
    }
}
