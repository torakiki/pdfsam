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
package org.pdfsam.alternatemix;

import org.pdfsam.support.params.AbstractPdfOutputParametersBuilder;
import org.pdfsam.support.params.SingleOutputTaskParametersBuilder;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.input.PdfMixInput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.AlternateMixParameters;

/**
 * Builder for the {@link AlternateMixParameters}
 * 
 * @author Andrea Vacondio
 *
 */
class AlternateMixParametersBuilder extends AbstractPdfOutputParametersBuilder<AlternateMixParameters> implements
        SingleOutputTaskParametersBuilder<AlternateMixParameters> {

    private FileTaskOutput output;
    private PdfFileSource firstSource;
    private PdfFileSource secondSource;
    private boolean reverseFirst;
    private boolean reverseSecond;
    private int stepFirst = 1;
    private int stepSecond = 1;

    @Override
    public void output(FileTaskOutput output) {
        this.output = output;
    }

    AlternateMixParametersBuilder first(PdfFileSource firstSource) {
        this.firstSource = firstSource;
        return this;
    }

    AlternateMixParametersBuilder second(PdfFileSource secondSource) {
        this.secondSource = secondSource;
        return this;
    }

    AlternateMixParametersBuilder reverseFirst(boolean reverseFirst) {
        this.reverseFirst = reverseFirst;
        return this;
    }

    AlternateMixParametersBuilder reverseSecond(boolean reverseSecond) {
        this.reverseSecond = reverseSecond;
        return this;
    }

    AlternateMixParametersBuilder stepFirst(int stepFirst) {
        this.stepFirst = stepFirst;
        return this;
    }

    AlternateMixParametersBuilder stepSecond(int stepSecond) {
        this.stepSecond = stepSecond;
        return this;
    }

    @Override
    public AlternateMixParameters build() {
        PdfMixInput firstInput = new PdfMixInput(firstSource, reverseFirst, stepFirst);
        PdfMixInput secondInput = new PdfMixInput(secondSource, reverseSecond, stepSecond);
        AlternateMixParameters params = new AlternateMixParameters(firstInput, secondInput);
        params.setCompress(isCompress());
        params.setExistingOutputPolicy(existingOutput());
        params.setVersion(getVersion());
        params.setOutput(output);
        return params;
    }
}
