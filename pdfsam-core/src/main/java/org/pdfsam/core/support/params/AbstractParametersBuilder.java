/*
 * This file is part of the PDF Split And Merge source code
 * Created on 24/giu/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.core.support.params;

import org.apache.commons.lang3.builder.Builder;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.parameter.base.AbstractParameters;

/**
 * Abstract builder for {@link AbstractParameters}.
 * 
 * @author Andrea Vacondio
 * @param <P>
 *            type of the parameters this builder builds
 */
public abstract class AbstractParametersBuilder<P extends AbstractParameters> implements Builder<P> {

    private ExistingOutputPolicy existingOutput = ExistingOutputPolicy.RENAME;

    public void existingOutput(ExistingOutputPolicy existingOutput) {
        this.existingOutput = existingOutput;
    }

    protected ExistingOutputPolicy existingOutput() {
        return existingOutput;
    }

}
