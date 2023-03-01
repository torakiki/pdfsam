/*
 * This file is part of the PDF Split And Merge source code
 * Created on 26/giu/2014
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
package org.pdfsam.core.support.params;

import org.apache.commons.lang3.builder.Builder;
import org.sejda.model.output.SingleOrMultipleTaskOutput;
import org.sejda.model.parameter.base.MultipleOutputTaskParameters;
import org.sejda.model.parameter.base.SingleOrMultipleOutputTaskParameters;

/**
 * Builder for a {@link MultipleOutputTaskParameters}
 * 
 * @author Andrea Vacondio
 * @param <P>
 *            type of parameters built
 *
 */
public interface MultipleOutputTaskParametersBuilder<P extends SingleOrMultipleOutputTaskParameters>
        extends Builder<P> {

    void prefix(String prefix);

    void output(SingleOrMultipleTaskOutput output);
}
