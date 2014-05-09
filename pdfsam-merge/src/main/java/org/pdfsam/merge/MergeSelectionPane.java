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

import static org.pdfsam.support.RequireUtils.requireNotNull;

import java.util.function.Consumer;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.selection.EncryptionStatusColumn;
import org.pdfsam.ui.selection.FileColumn;
import org.pdfsam.ui.selection.LongColumn;
import org.pdfsam.ui.selection.SelectionPane;
import org.pdfsam.ui.selection.SelectionTableColumn;
import org.pdfsam.ui.selection.StringColumn;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.input.PdfMergeInput;
import org.sejda.model.parameter.MergeParameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Selection panel for the merge module.
 * 
 * @author Andrea Vacondio
 *
 */
public class MergeSelectionPane extends SelectionPane<MergeParameters> {
    private static final Logger LOG = LoggerFactory.getLogger(MergeSelectionPane.class);

    public MergeSelectionPane(String ownerModule) {
        super(ownerModule, new SelectionTableColumn<?>[] { new EncryptionStatusColumn(ownerModule), FileColumn.NAME,
                LongColumn.SIZE, LongColumn.PAGES, LongColumn.LAST_MODIFIED, StringColumn.PAGE_SELECTION });
    }

    public void apply(MergeParameters params, Consumer<String> onError) {
        requireNotNull(params, "Cannot set input on a null parameter instance");
        if (table().getItems().isEmpty()) {
            onError.accept(DefaultI18nContext.getInstance().i18n("No pdf document has been selected"));
        }
        try {
            table().getItems().stream().map(i -> new PdfMergeInput(i.toPdfFileSource(), i.toPageRangeSet()))
                    .forEach(params::addInput);
        } catch (ConversionException e) {
            LOG.error(e.getMessage());
            onError.accept(e.getMessage());
        }
    }

}
