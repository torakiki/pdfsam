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
package org.pdfsam.ui.io;

import java.util.function.Consumer;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.sejda.conversion.DirectoryOutputAdapter;
import org.sejda.model.parameter.base.MultipleOutputTaskParameters;

/**
 * A {@link BrowsableDirectoryField} letting the user select a directory as output for a {@link MultipleOutputTaskParametersBuilder}.
 * 
 * @author Andrea Vacondio
 *
 */
public class BrowsableOutputDirectoryField extends BrowsableDirectoryField implements
        TaskParametersBuildStep<MultipleOutputTaskParametersBuilder<?>> {

    public BrowsableOutputDirectoryField() {
        super(false);
    }

    @Override
    public void apply(MultipleOutputTaskParametersBuilder<? extends MultipleOutputTaskParameters> builder,
            Consumer<String> onError) {
        getTextField().validate();
        if (getTextField().getValidationState() == ValidationState.VALID) {
            builder.output(new DirectoryOutputAdapter(getTextField().getText()).getPdfDirectoryOutput());
        } else {
            onError.accept(DefaultI18nContext.getInstance().i18n("The selected output directory is invalid"));
        }
    }

}
