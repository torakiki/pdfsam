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
package org.pdfsam.support.params;

import org.apache.commons.lang3.builder.Builder;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.base.SingleOutputTaskParameters;

/**
 * Builder for a {@link SingleOutputTaskParameters}
 * 
 * @author Andrea Vacondio
 * @param <P>
 *            type of parameters built
 */
public interface SingleOutputTaskParametersBuilder<P extends SingleOutputTaskParameters> extends Builder<P> {

    void output(FileTaskOutput output);
}
