/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/mag/2014
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
package org.pdfsam.ui.io;

import java.util.function.Consumer;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.io.FileType;
import org.pdfsam.support.params.SingleOutputTaskParametersBuilder;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper.OpenType;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.sejda.conversion.FileOutputAdapter;
import org.sejda.model.parameter.base.SingleOutputTaskParameters;

/**
 * A {@link BrowsableFileField} letting the user select a PDF document as output for a {@link SingleOutputTaskParameters}.
 * 
 * @author Andrea Vacondio
 *
 */
public class BrowsablePdfOutputField extends BrowsableFileField implements
        TaskParametersBuildStep<SingleOutputTaskParametersBuilder<?>> {

    public BrowsablePdfOutputField() {
        super(FileType.PDF, OpenType.SAVE);
        this.enforceValidation(false, false);
    }

    @Override
    public void apply(SingleOutputTaskParametersBuilder<?> builder, Consumer<String> onError) {
        getTextField().validate();
        if (getTextField().getValidationState() == ValidationState.VALID) {
            builder.output(new FileOutputAdapter(getTextField().getText()).getFileOutput());
        } else {
            onError.accept(DefaultI18nContext.getInstance().i18n("The selected PDF file is invalid"));
        }
    }

}
