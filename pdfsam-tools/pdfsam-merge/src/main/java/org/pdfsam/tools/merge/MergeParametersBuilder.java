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
package org.pdfsam.tools.merge;

import org.pdfsam.core.support.params.AbstractPdfOutputParametersBuilder;
import org.pdfsam.core.support.params.SingleOutputTaskParametersBuilder;
import org.sejda.commons.collection.NullSafeSet;
import org.sejda.model.input.PdfMergeInput;
import org.sejda.model.outline.OutlinePolicy;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.MergeParameters;
import org.sejda.model.pdf.form.AcroFormPolicy;
import org.sejda.model.scale.PageNormalizationPolicy;
import org.sejda.model.toc.ToCPolicy;

import java.util.Set;

/**
 * Builder for {@link MergeParameters}
 *
 * @author Andrea Vacondio
 */
class MergeParametersBuilder extends AbstractPdfOutputParametersBuilder<MergeParameters>
        implements SingleOutputTaskParametersBuilder<MergeParameters> {

    private final Set<PdfMergeInput> inputs = new NullSafeSet<>();
    private OutlinePolicy outlinePolicy = OutlinePolicy.RETAIN;
    private boolean blankIfOdd;
    private boolean footer;
    private PageNormalizationPolicy pageNormalizationPolicy = PageNormalizationPolicy.NONE;
    private AcroFormPolicy formsPolicy = AcroFormPolicy.MERGE;
    private ToCPolicy tocPolicy = ToCPolicy.NONE;
    private FileTaskOutput output;

    void addInput(PdfMergeInput input) {
        this.inputs.add(input);
    }

    boolean hasInput() {
        return !inputs.isEmpty();
    }

    void outlinePolicy(OutlinePolicy outlinePolicy) {
        this.outlinePolicy = outlinePolicy;
    }

    void blankPageIfOdd(boolean blankIfOdd) {
        this.blankIfOdd = blankIfOdd;
    }

    void footer(boolean footer) {
        this.footer = footer;
    }

    void pageNormalizationPolicy(PageNormalizationPolicy pageNormalizationPolicy) {
        this.pageNormalizationPolicy = pageNormalizationPolicy;
    }

    void acroFormsPolicy(AcroFormPolicy formsPolicy) {
        this.formsPolicy = formsPolicy;
    }

    void tocPolicy(ToCPolicy tocPolicy) {
        this.tocPolicy = tocPolicy;
    }

    @Override
    public void output(FileTaskOutput output) {
        this.output = output;
    }

    @Override
    public MergeParameters build() {
        MergeParameters params = new MergeParameters();
        params.setCompress(isCompress());
        params.setExistingOutputPolicy(existingOutput());
        params.setVersion(getVersion());
        inputs.forEach(params::addInput);
        params.setOutlinePolicy(outlinePolicy);
        params.setBlankPageIfOdd(blankIfOdd);
        params.setAcroFormPolicy(formsPolicy);
        params.setTableOfContentsPolicy(tocPolicy);
        params.setOutput(output);
        params.setFilenameFooter(footer);
        params.setPageNormalizationPolicy(pageNormalizationPolicy);
        return params;
    }

}
