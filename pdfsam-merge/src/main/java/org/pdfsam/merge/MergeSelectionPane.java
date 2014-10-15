/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/apr/2014
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
package org.pdfsam.merge;

import java.util.function.Consumer;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.selection.multiple.FileColumn;
import org.pdfsam.ui.selection.multiple.IntColumn;
import org.pdfsam.ui.selection.multiple.LoadingColumn;
import org.pdfsam.ui.selection.multiple.LongColumn;
import org.pdfsam.ui.selection.multiple.MultipleSelectionPane;
import org.pdfsam.ui.selection.multiple.SelectionTableColumn;
import org.pdfsam.ui.selection.multiple.StringColumn;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.input.PdfMergeInput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Selection panel for the merge module.
 * 
 * @author Andrea Vacondio
 *
 */
public class MergeSelectionPane extends MultipleSelectionPane implements
        TaskParametersBuildStep<MergeParametersBuilder> {
    private static final Logger LOG = LoggerFactory.getLogger(MergeSelectionPane.class);

    public MergeSelectionPane(String ownerModule) {
        super(ownerModule, new SelectionTableColumn<?>[] { new LoadingColumn(ownerModule), FileColumn.NAME,
                LongColumn.SIZE, IntColumn.PAGES, LongColumn.LAST_MODIFIED, StringColumn.PAGE_SELECTION });
    }

    public void apply(MergeParametersBuilder builder, Consumer<String> onError) {
        if (!table().getItems().isEmpty()) {
            try {
                table().getItems().stream().map(i -> new PdfMergeInput(i.toPdfFileSource(), i.toPageRangeSet()))
                        .forEach(builder::addInput);
            } catch (ConversionException e) {
                LOG.error(e.getMessage());
                onError.accept(e.getMessage());
            }
        } else {
            onError.accept(DefaultI18nContext.getInstance().i18n("No pdf document has been selected"));
        }

    }
}
