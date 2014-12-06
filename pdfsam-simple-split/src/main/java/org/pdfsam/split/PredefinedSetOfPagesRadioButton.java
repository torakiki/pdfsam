/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/giu/2014
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
package org.pdfsam.split;

import static org.pdfsam.support.RequireUtils.requireNotNull;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import javafx.scene.control.RadioButton;

import org.pdfsam.support.params.SinglePdfSourceMultipleOutputParametersBuilder;
import org.pdfsam.ui.workspace.RestorableView;
import org.sejda.model.parameter.SimpleSplitParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;

/**
 * A radio button associated to a Predefined set of pages
 * 
 * @author Andrea Vacondio
 *
 */
class PredefinedSetOfPagesRadioButton extends RadioButton implements SplitParametersBuilderCreator, RestorableView {

    private PredefinedSetOfPages pages;

    public PredefinedSetOfPagesRadioButton(PredefinedSetOfPages pages, String text) {
        super(text);
        requireNotNull(pages, "Cannot create the radio button with a null predefined set of pages.");
        this.pages = pages;
    }

    public PredefinedSetOfPages getPages() {
        return pages;
    }

    public void saveStateTo(Map<String, String> data) {
        if (isSelected()) {
            data.put(pages.toString(), Boolean.TRUE.toString());
        }
    }

    public void restoreStateFrom(Map<String, String> data) {
        Optional.ofNullable(data.get(pages.toString())).map(Boolean::valueOf).ifPresent(this::setSelected);
    }

    public SimpleSplitParametersBuilder getBuilder(Consumer<String> onError) {
        SimpleSplitParametersBuilder builder = new SimpleSplitParametersBuilder();
        builder.pages(pages);
        return builder;
    }

    /**
     * Builder for the {@link SimpleSplitParameters}
     * 
     * @author Andrea Vacondio
     *
     */
    static class SimpleSplitParametersBuilder extends
            SinglePdfSourceMultipleOutputParametersBuilder<SimpleSplitParameters> {

        private PredefinedSetOfPages pages;

        void pages(PredefinedSetOfPages pages) {
            this.pages = pages;
        }

        public SimpleSplitParameters build() {
            SimpleSplitParameters params = new SimpleSplitParameters(pages);
            params.setCompress(isCompress());
            params.setOverwrite(isOverwrite());
            params.setVersion(getVersion());
            params.setOutput(getOutput());
            params.setOutputPrefix(getPrefix());
            params.setSource(getSource());
            return params;
        }
    }

}
