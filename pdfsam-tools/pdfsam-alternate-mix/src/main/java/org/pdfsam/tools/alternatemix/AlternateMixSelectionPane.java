/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30 ago 2016
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.tools.alternatemix;

import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.components.selection.multiple.FileColumn;
import org.pdfsam.ui.components.selection.multiple.IntColumn;
import org.pdfsam.ui.components.selection.multiple.LoadingColumn;
import org.pdfsam.ui.components.selection.multiple.LongColumn;
import org.pdfsam.ui.components.selection.multiple.MultipleSelectionPane;
import org.pdfsam.ui.components.selection.multiple.PaceColumn;
import org.pdfsam.ui.components.selection.multiple.PageRangesColumn;
import org.pdfsam.ui.components.selection.multiple.ReverseColumn;
import org.pdfsam.ui.components.selection.multiple.SelectedPagesColumn;
import org.pdfsam.ui.components.selection.multiple.SelectionTableRowData;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.input.PdfMixInput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.defaultIfBlank;
import static org.apache.commons.lang3.StringUtils.trim;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
public class AlternateMixSelectionPane extends MultipleSelectionPane
        implements TaskParametersBuildStep<AlternateMixParametersBuilder> {

    private static final Logger LOG = LoggerFactory.getLogger(AlternateMixSelectionPane.class);

    public AlternateMixSelectionPane(String ownerModule) {
        super(ownerModule, true, true, new LoadingColumn(ownerModule), FileColumn.NAME, LongColumn.SIZE,
                IntColumn.PAGES, new PageRangesColumn(
                        i18n().tr("Double click to set pages you want to mix (ex: 2 or 5-23 or 2,5-7,12-)")),
                new SelectedPagesColumn(), new PaceColumn(), new ReverseColumn(), LongColumn.LAST_MODIFIED);
        this.showTotalPagesLabel();
    }

    @Override
    public void apply(AlternateMixParametersBuilder builder, Consumer<String> onError) {
        try {
            var inputs = table().getItems().stream().filter(s -> !Objects.equals("0", trim(s.pageSelection.get())))
                    .toList();
            if (inputs.isEmpty()) {
                onError.accept(i18n().tr("No PDF document has been selected"));
            } else {
                for (SelectionTableRowData row : inputs) {
                    String step = defaultIfBlank(row.pace.get(), "1").trim();
                    if (step.matches("[1-9]\\d*")) {
                        PdfMixInput input = new PdfMixInput(row.descriptor().toPdfFileSource(), row.reverse.get(),
                                Integer.parseInt(step));
                        input.addAllPageRanges(row.toPageRangeSet());
                        builder.addInput(input);
                    } else {
                        onError.accept(i18n().tr("Select a positive integer number as pace"));
                        break;
                    }
                }
            }
        } catch (ConversionException e) {
            LOG.error(e.getMessage());
            onError.accept(e.getMessage());
        }
    }

}
