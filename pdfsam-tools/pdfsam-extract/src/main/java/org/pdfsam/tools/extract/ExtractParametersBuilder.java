/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/giu/2014
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
package org.pdfsam.tools.extract;

import org.pdfsam.core.support.params.MultiplePdfSourceMultipleOutputParametersBuilder;
import org.sejda.model.optimization.OptimizationPolicy;
import org.sejda.model.parameter.ExtractPagesParameters;
import org.sejda.model.pdf.page.PageRange;

import java.util.Set;

import static org.pdfsam.core.ConfigurableSystemProperty.PDFSAM_DISABLE_SPLIT_OPTIMIZATION;

/**
 * Builder for {@link ExtractPagesParameters}
 * 
 * @author Andrea Vacondio
 *
 */
class ExtractParametersBuilder extends MultiplePdfSourceMultipleOutputParametersBuilder<ExtractPagesParameters> {

    private OptimizationPolicy optimizationPolicy = OptimizationPolicy.AUTO;
    private Set<PageRange> ranges;
    private boolean invertSelection = false;
    private boolean separateForEachRange = false;

    public void optimizationPolicy(OptimizationPolicy optimizationPolicy) {
        this.optimizationPolicy = optimizationPolicy;
    }

    public OptimizationPolicy getOptimizationPolicy() {
        if (!Boolean.getBoolean(PDFSAM_DISABLE_SPLIT_OPTIMIZATION)) {
            return optimizationPolicy;
        }
        return OptimizationPolicy.NO;
    }

    public void ranges(Set<PageRange> ranges) {
        this.ranges = ranges;
    }

    public void invertSelection(boolean invertSelection) {
        this.invertSelection = invertSelection;
    }

    public void separateForEachRange(boolean separateForEachRange) {
        this.separateForEachRange = separateForEachRange;
    }

    @Override
    public ExtractPagesParameters build() {
        ExtractPagesParameters params = new ExtractPagesParameters();
        params.setCompress(isCompress());
        params.setExistingOutputPolicy(existingOutput());
        params.setVersion(getVersion());
        params.setOutput(getOutput());
        params.setOptimizationPolicy(getOptimizationPolicy());
        params.discardOutline(isDiscardBookmarks());
        params.addAllPageRanges(ranges);
        params.setOutputPrefix(getPrefix());
        params.setInvertSelection(invertSelection);
        params.setSeparateFileForEachRange(separateForEachRange);
        getInputs().forEach(params::addSource);
        return params;
    }

}
