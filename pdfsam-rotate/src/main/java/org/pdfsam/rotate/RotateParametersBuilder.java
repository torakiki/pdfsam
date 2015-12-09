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
package org.pdfsam.rotate;

import org.pdfsam.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.support.params.MultiplePdfSourceMultipleOutputParametersBuilder;
import org.sejda.model.parameter.RotateParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;

/**
 * Builder for {@link RotateParameters}
 * 
 * @author Andrea Vacondio
 *
 */
class RotateParametersBuilder extends MultiplePdfSourceMultipleOutputParametersBuilder<RotateParameters> implements
        MultipleOutputTaskParametersBuilder<RotateParameters> {

    private PredefinedSetOfPages rotationType;
    private Rotation rotation;

    public void rotation(Rotation rotation) {
        this.rotation = rotation;
    }

    public void rotationType(PredefinedSetOfPages rotationType) {
        this.rotationType = rotationType;
    }

    public RotateParameters build() {
        RotateParameters params = new RotateParameters(rotation, rotationType);
        params.setCompress(isCompress());
        params.setExistingOutputPolicy(existingOutput());
        params.setVersion(getVersion());
        params.setOutput(getOutput());
        params.setOutputPrefix(getPrefix());
        getInputs().forEach(params::addSource);
        return params;
    }

}
