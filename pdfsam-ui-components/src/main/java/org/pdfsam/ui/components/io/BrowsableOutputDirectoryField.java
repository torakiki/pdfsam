/*
 * This file is part of the PDF Split And Merge source code
 * Created on 26/giu/2014
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

import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.model.ui.NonExistingOutputDirectoryEvent;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.sejda.model.parameter.base.SingleOrMultipleOutputTaskParameters;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.function.Consumer;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.core.context.StringPersistentProperty.WORKING_PATH;
import static org.pdfsam.core.support.validation.Validators.and;
import static org.pdfsam.core.support.validation.Validators.nonBlank;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.model.output.FileOrDirectoryTaskOutput.directory;

/**
 * A {@link BrowsableDirectoryField} letting the user select a directory as output for a {@link MultipleOutputTaskParametersBuilder}.
 *
 * @author Andrea Vacondio
 */
public class BrowsableOutputDirectoryField extends BrowsableDirectoryField
        implements TaskParametersBuildStep<MultipleOutputTaskParametersBuilder<?>> {

    public BrowsableOutputDirectoryField() {
        this(app());
    }

    BrowsableOutputDirectoryField(ApplicationContext context) {
        context.persistentSettings().get(WORKING_PATH).ifPresent(getTextField()::setText);
        getTextField().setValidator(and(nonBlank(), v -> !Files.isRegularFile(Paths.get(v))));
        setFieldPromptAndAccessibleText(i18n().tr("Output directory"));
        setBrowseButtonAccessibleText(i18n().tr("Browse for output directory"));
    }

    @Override
    public void apply(MultipleOutputTaskParametersBuilder<? extends SingleOrMultipleOutputTaskParameters> builder,
            Consumer<String> onError) {
        getTextField().validate();
        if (getTextField().getValidationState() == FXValidationSupport.ValidationState.VALID) {
            var output = Paths.get(getTextField().getText());
            if (!Files.exists(output)) {
                eventStudio().broadcast(new NonExistingOutputDirectoryEvent(output));
            }
            if (Files.isDirectory(output)) {
                builder.output(directory(output.toFile()));
            } else {
                onError.accept(i18n().tr("An existing output directory is required"));
            }
        } else {
            onError.accept(i18n().tr("The output directory is required"));
        }
    }
}
