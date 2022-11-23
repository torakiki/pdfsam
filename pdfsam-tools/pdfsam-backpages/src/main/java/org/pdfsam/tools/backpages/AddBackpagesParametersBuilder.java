package org.pdfsam.tools.backpages;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22/11/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import org.pdfsam.core.support.params.AbstractPdfOutputParametersBuilder;
import org.pdfsam.core.support.params.SinglePdfSourceTaskParametersBuilder;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.SingleOrMultipleTaskOutput;
import org.sejda.model.parameter.AddBackPagesParameters;
import org.sejda.model.pdf.page.PageRange;

import java.util.Set;

import static java.util.Objects.nonNull;

/**
 * @author Andrea Vacondio
 */
class AddBackpagesParametersBuilder extends AbstractPdfOutputParametersBuilder<AddBackPagesParameters>
        implements SinglePdfSourceTaskParametersBuilder<AddBackPagesParameters> {

    private PdfFileSource source;
    private SingleOrMultipleTaskOutput output;
    private PdfFileSource backPagesSource;
    private int step = 1;
    private Set<PageRange> ranges;

    @Override
    public void source(PdfFileSource source) {
        this.source = source;
    }

    public void backPagesSource(PdfFileSource backPagesSource) {
        this.backPagesSource = backPagesSource;
    }

    public void output(SingleOrMultipleTaskOutput output) {
        this.output = output;
    }

    public void step(int step) {
        this.step = step;
    }

    public void ranges(Set<PageRange> ranges) {
        this.ranges = ranges;
    }

    @Override
    public AddBackPagesParameters build() {
        AddBackPagesParameters params = new AddBackPagesParameters();
        params.setCompress(isCompress());
        params.setExistingOutputPolicy(existingOutput());
        params.setVersion(getVersion());
        params.setOutput(output);
        params.setBackPagesSource(backPagesSource);
        params.setStep(step);
        params.addSource(source);
        if (nonNull(ranges)) {
            params.addAllPageRanges(ranges);
        }
        return params;
    }
}
