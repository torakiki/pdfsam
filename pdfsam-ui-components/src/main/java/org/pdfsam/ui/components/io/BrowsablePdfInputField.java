/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/mag/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.io;

import org.pdfsam.core.support.params.SinglePdfSourceTaskParametersBuilder;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.model.io.FileType;
import org.pdfsam.model.io.OpenType;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.sejda.conversion.PdfFileSourceAdapter;
import org.sejda.model.parameter.base.SinglePdfSourceTaskParameters;

import java.util.function.Consumer;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * A {@link BrowsableFileField} letting the user select a PDF document as input for a {@link SinglePdfSourceTaskParameters}.
 * 
 * @author Andrea Vacondio
 *
 */
public class BrowsablePdfInputField extends BrowsableFileField
        implements TaskParametersBuildStep<SinglePdfSourceTaskParametersBuilder<?>> {

    public BrowsablePdfInputField() {
        super(FileType.PDF, OpenType.OPEN);
        setFieldPromptAndAccessibleText(i18n().tr("Select a PDF file"));
        setBrowseButtonAccessibleText(i18n().tr("Browse for source PDF file"));
    }

    @Override
    public void apply(SinglePdfSourceTaskParametersBuilder<?> builder, Consumer<String> onError) {
        getTextField().validate();
        if (getTextField().getValidationState() == FXValidationSupport.ValidationState.VALID) {
            builder.source(new PdfFileSourceAdapter(getTextField().getText()).getPdfFileSource());
        } else {
            onError.accept(i18n().tr("The selected PDF file is invalid"));
        }
    }
}
