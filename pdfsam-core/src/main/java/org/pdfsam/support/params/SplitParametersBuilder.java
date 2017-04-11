/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05 feb 2016
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
package org.pdfsam.support.params;

import org.sejda.model.optimization.OptimizationPolicy;
import org.sejda.model.parameter.base.MultiplePdfSourceMultipleOutputParameters;

/**
 * Builder for split tasks parameters
 * 
 * @author Andrea Vacondio
 *
 */
public abstract class SplitParametersBuilder<P extends MultiplePdfSourceMultipleOutputParameters>
        extends SinglePdfSourceMultipleOutputParametersBuilder<P> {

    private OptimizationPolicy optimizationPolicy = OptimizationPolicy.AUTO;

    public void optimizationPolicy(OptimizationPolicy optimizationPolicy) {
        this.optimizationPolicy = optimizationPolicy;
    }

    public OptimizationPolicy getOptimizationPolicy() {
        if (!Boolean.getBoolean(PDFSAM_DISABLE_SPLIT_OPTIMIZATION)) {
            return optimizationPolicy;
        }
        return OptimizationPolicy.NO;
    }

}
