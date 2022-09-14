/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23/giu/2014
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
package org.pdfsam.ui.selection.single;

import java.util.function.Consumer;

import org.pdfsam.i18n.I18nContext;
import org.pdfsam.support.params.SinglePdfSourceTaskParametersBuilder;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.sejda.model.parameter.base.MultiplePdfSourceMultipleOutputParameters;
import org.sejda.model.parameter.base.SinglePdfSourceTaskParameters;

/**
 * {@link SingleSelectionPane} capable of participating as a {@link TaskParametersBuildStep} to a {@link SinglePdfSourceTaskParameters} build process.
 * 
 * @author Andrea Vacondio
 *
 */
public class TaskParametersBuilderSingleSelectionPane extends SingleSelectionPane implements
        TaskParametersBuildStep<SinglePdfSourceTaskParametersBuilder<? extends MultiplePdfSourceMultipleOutputParameters>> {

    public TaskParametersBuilderSingleSelectionPane(String ownerModule) {
        super(ownerModule);
    }

    @Override
    public void apply(SinglePdfSourceTaskParametersBuilder<? extends MultiplePdfSourceMultipleOutputParameters> builder,
            Consumer<String> onError) {
        getField().getTextField().validate();
        if (getField().getTextField().getValidationState() == ValidationState.VALID) {
            builder.source(getPdfDocumentDescriptor().toPdfFileSource());
        } else {
            onError.accept(I18nContext.getInstance().i18n("The selected PDF document is invalid"));
        }
    }
}
