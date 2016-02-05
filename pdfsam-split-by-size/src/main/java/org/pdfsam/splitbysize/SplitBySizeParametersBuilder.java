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
package org.pdfsam.splitbysize;

import org.pdfsam.support.params.SplitParametersBuilder;
import org.sejda.model.parameter.SplitBySizeParameters;

/**
 * Builder for {@link SplitBySizeParameters}
 * 
 * @author Andrea Vacondio
 *
 */
class SplitBySizeParametersBuilder extends SplitParametersBuilder<SplitBySizeParameters> {

    private long size;

    void size(long size) {
        this.size = size;
    }

    public SplitBySizeParameters build() {
        SplitBySizeParameters params = new SplitBySizeParameters(size);
        params.setCompress(isCompress());
        params.setExistingOutputPolicy(existingOutput());
        params.setVersion(getVersion());
        params.setOutput(getOutput());
        params.setOutputPrefix(getPrefix());
        params.setSource(getSource());
        params.setOptimizationPolicy(getOptimizationPolicy());
        return params;
    }
}
