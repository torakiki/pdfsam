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

import org.pdfsam.core.support.params.SingleOutputTaskParametersBuilder;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.model.io.FileType;
import org.pdfsam.model.io.OpenType;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.base.SingleOutputTaskParameters;

import java.io.File;
import java.nio.file.Paths;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.i18n.I18nContext.i18n;

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
        setFieldPromptAndAccessibleText(i18n().tr("Select a PDF destination file"));
        setBrowseButtonAccessibleText(i18n().tr("Browse for destination PDF file"));
    }

    @Override
    public void apply(SingleOutputTaskParametersBuilder<?> builder, Consumer<String> onError) {
        String output = getTextField().getText();
        if (isNotBlank(output)) {
            var path = Paths.get(output);
            //if not absolute, resolve against working path
            if (!path.isAbsolute()) {
                getTextField().setText(
                        app().runtimeState().workingPathValue().map(w -> w.resolve(output)).orElse(path).toString());
            }
        }
        getTextField().validate();
        if (getTextField().getValidationState() == FXValidationSupport.ValidationState.VALID) {
            builder.output(new FileTaskOutput(new File(getTextField().getText())));
        } else {
            onError.accept(i18n().tr("A .pdf destination file extension is required"));
        }
    }

}
