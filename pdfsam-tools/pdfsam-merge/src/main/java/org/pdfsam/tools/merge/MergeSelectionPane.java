/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/apr/2014
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
package org.pdfsam.tools.merge;

import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.components.selection.multiple.FileColumn;
import org.pdfsam.ui.components.selection.multiple.IntColumn;
import org.pdfsam.ui.components.selection.multiple.LoadingColumn;
import org.pdfsam.ui.components.selection.multiple.LongColumn;
import org.pdfsam.ui.components.selection.multiple.MultipleSelectionPane;
import org.pdfsam.ui.components.selection.multiple.PageRangesColumn;
import org.pdfsam.ui.components.selection.multiple.SelectedPagesColumn;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.input.PdfMergeInput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.trim;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Selection panel for the merge module.
 *
 * @author Andrea Vacondio
 */
public class MergeSelectionPane extends MultipleSelectionPane
        implements TaskParametersBuildStep<MergeParametersBuilder> {
    private static final Logger LOG = LoggerFactory.getLogger(MergeSelectionPane.class);

    public MergeSelectionPane(String ownerModule) {
        super(ownerModule, true, true, new LoadingColumn(ownerModule), FileColumn.NAME, LongColumn.SIZE,
                IntColumn.PAGES, new PageRangesColumn(
                        i18n().tr("Double click to set pages you want to merge (ex: 2 or 5-23 or 2,5-7,12-)")),
                new SelectedPagesColumn(), LongColumn.LAST_MODIFIED);
        this.showTotalPagesLabel();
    }

    @Override
    public void apply(MergeParametersBuilder builder, Consumer<String> onError) {
        try {
            table().getItems().stream().filter(s -> !Objects.equals("0", trim(s.pageSelection.get())))
                    .map(i -> new PdfMergeInput(i.descriptor().toPdfFileSource(), i.toPageRangeSet()))
                    .forEach(builder::addInput);
            if (!builder.hasInput()) {
                onError.accept(i18n().tr("No PDF document has been selected"));
            }
        } catch (ConversionException e) {
            LOG.error(e.getMessage());
            onError.accept(e.getMessage());
        }
    }
}
