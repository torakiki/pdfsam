/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28 ago 2016
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
package org.pdfsam.tools.extract;

import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.components.selection.multiple.FileColumn;
import org.pdfsam.ui.components.selection.multiple.IntColumn;
import org.pdfsam.ui.components.selection.multiple.LoadingColumn;
import org.pdfsam.ui.components.selection.multiple.LongColumn;
import org.pdfsam.ui.components.selection.multiple.MultipleSelectionPane;
import org.sejda.conversion.exception.ConversionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.function.Consumer;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Selection pane for the extract module
 *
 * @author Andrea Vacondio
 */
public class ExtractSelectionPane extends MultipleSelectionPane
        implements TaskParametersBuildStep<ExtractParametersBuilder> {

    private static final Logger LOG = LoggerFactory.getLogger(ExtractSelectionPane.class);

    public ExtractSelectionPane(String ownerModule) {
        super(ownerModule, false, false, new LoadingColumn(ownerModule), FileColumn.NAME, LongColumn.SIZE,
                IntColumn.PAGES, LongColumn.LAST_MODIFIED);
    }

    @Override
    public void apply(ExtractParametersBuilder builder, Consumer<String> onError) {
        try {
            table().getItems().forEach(i -> builder.addSource(i.descriptor().toPdfFileSource()));
            if (!builder.hasInput()) {
                onError.accept(i18n().tr("No PDF document has been selected"));
            }
        } catch (ConversionException e) {
            LOG.error(e.getMessage());
            onError.accept(e.getMessage());
        }
    }
}
