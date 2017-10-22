/*
 * Created on 20 giu 2016
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
package org.pdfsam.task;

import static java.util.Objects.requireNonNull;
import static java.util.Optional.ofNullable;

import java.util.Arrays;
import java.util.Set;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.sejda.common.collection.NullSafeSet;
import org.sejda.model.input.PdfSource;
import org.sejda.model.pdf.page.PagesSelection;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;
import org.sejda.model.validation.constraint.NotEmpty;

/**
 * Input source for a rotation task
 * 
 * @author Andrea Vacondio
 */
public class PdfRotationInput implements PagesSelection {

    @Valid
    @NotNull
    public final PdfSource<?> source;
    @Valid
    @NotNull
    public final Rotation rotation;
    @Valid
    @NotEmpty
    private final Set<PagesSelection> pageSelection = new NullSafeSet<>();

    /**
     * @param source
     * @param rotation
     * @param pages
     *            the pages selection to apply the rotation. If no page selection is specified, all pages are rotated
     */
    public PdfRotationInput(PdfSource<?> source, Rotation rotation, PagesSelection... pages) {
        requireNonNull(rotation, "A rotation is expected");
        requireNonNull(source, "A valid source is expected");
        this.source = source;
        this.rotation = rotation;
        this.pageSelection.addAll(ofNullable(pages).filter(p -> p.length > 0).map(Arrays::asList)
                .orElseGet(() -> Arrays.asList(PredefinedSetOfPages.ALL_PAGES)));
    }

    @Override
    public Set<Integer> getPages(int totalNumberOfPage) {
        Set<Integer> retSet = new NullSafeSet<>();
        for (PagesSelection selection : pageSelection) {
            retSet.addAll(selection.getPages(totalNumberOfPage));
        }
        return retSet;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("source", source).append("pageSelection", pageSelection)
                .append("rotation", rotation).toString();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(source).append(pageSelection).append(rotation).toHashCode();
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof PdfRotationInput)) {
            return false;
        }
        PdfRotationInput input = (PdfRotationInput) other;
        return new EqualsBuilder().append(source, input.source).append(rotation, input.rotation)
                .append(pageSelection, input.pageSelection).isEquals();
    }
}
