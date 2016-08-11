/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22 giu 2016
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

import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.base.SinglePdfSourceSingleOutputParameters;

/**
 * Abstract builder for {@link SinglePdfSourceSingleOutputParameters}.
 * 
 * @author Andrea Vacondio
 * @param <P>
 *            type of the parameters the builder builds
 */
public abstract class SinglePdfSourceSingleOutputParametersBuilder<P extends SinglePdfSourceSingleOutputParameters>
        extends AbstractPdfOutputParametersBuilder<P>
        implements SinglePdfSourceTaskParametersBuilder<P>, SingleOutputTaskParametersBuilder<P> {

    private PdfFileSource source;
    private FileTaskOutput output;

    @Override
    public void source(PdfFileSource source) {
        this.source = source;
    }

    @Override
    public void output(FileTaskOutput output) {
        this.output = output;
    }

    protected PdfFileSource getSource() {
        return source;
    }

    protected FileTaskOutput getOutput() {
        return output;
    }

}
