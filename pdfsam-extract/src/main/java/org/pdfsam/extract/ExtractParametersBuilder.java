/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/giu/2014
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
package org.pdfsam.extract;

import java.util.Set;

import org.pdfsam.support.params.SinglePdfSourceSingleOutputParametersBuilder;
import org.sejda.model.optimization.OptimizationPolicy;
import org.sejda.model.parameter.ExtractPagesParameters;
import org.sejda.model.parameter.RotateParameters;
import org.sejda.model.pdf.page.PageRange;

/**
 * Builder for {@link RotateParameters}
 * 
 * @author Andrea Vacondio
 *
 */
class ExtractParametersBuilder extends SinglePdfSourceSingleOutputParametersBuilder<ExtractPagesParameters> {

    private OptimizationPolicy optimizationPolicy = OptimizationPolicy.AUTO;
    private Set<PageRange> ranges;

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

    @Override
    public ExtractPagesParameters build() {
        ExtractPagesParameters params = new ExtractPagesParameters();
        params.setCompress(isCompress());
        params.setExistingOutputPolicy(existingOutput());
        params.setVersion(getVersion());
        params.setOutput(getOutput());
        params.setSource(getSource());
        params.setOptimizationPolicy(getOptimizationPolicy());
        params.discardOutline(isDiscardBookmarks());
        params.addAllPageRanges(ranges);
        return params;
    }

}
