/*
 * Created on 21 giu 2016
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 * This file is part of Sejda.
 *
 * Sejda is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Sejda is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Sejda.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.model.task;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.sejda.model.output.SingleOrMultipleTaskOutput;
import org.sejda.model.parameter.base.AbstractPdfOutputParameters;
import org.sejda.model.parameter.base.SingleOrMultipleOutputTaskParameters;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Parameters to perform rotations to a multiple input PDF sources.
 * 
 * @author Andrea Vacondio
 *
 */
public class BulkRotateParameters extends AbstractPdfOutputParameters implements SingleOrMultipleOutputTaskParameters {

    private String outputPrefix = "";

    @NotEmpty
    @Valid
    private final Set<PdfRotationInput> inputSet = new HashSet<>();
    @Valid
    @NotNull
    private SingleOrMultipleTaskOutput output;

    @Override
    public SingleOrMultipleTaskOutput getOutput() {
        return output;
    }

    @Override
    public void setOutput(SingleOrMultipleTaskOutput output) {
        this.output = output;
    }

    /**
     * @return an unmodifiable view of the inputSet
     */
    public Set<PdfRotationInput> getInputSet() {
        return Collections.unmodifiableSet(inputSet);
    }

    public void addInput(PdfRotationInput input) {
        this.inputSet.add(input);
    }

    @Override
    public String getOutputPrefix() {
        return outputPrefix;
    }

    @Override
    public void setOutputPrefix(String outputPrefix) {
        this.outputPrefix = outputPrefix;
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().appendSuper(super.hashCode()).append(outputPrefix).append(inputSet).toHashCode();
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof BulkRotateParameters params)) {
            return false;
        }
        return new EqualsBuilder().appendSuper(super.equals(other)).append(outputPrefix, params.outputPrefix)
                .append(output, params.output).append(inputSet, params.inputSet).isEquals();
    }
}
