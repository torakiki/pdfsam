package org.pdfsam.model.ui;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 17/10/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import org.sejda.model.pdf.PdfVersion;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Default implementation for a combo item representing a {@link PdfVersion}
 *
 * @author Andrea Vacondio
 */
public class DefaultPdfVersionComboItem extends ComboItem<PdfVersion> implements PdfVersionComboItem {

    public DefaultPdfVersionComboItem(PdfVersion version) {
        super(version, i18n().tr("Version {0}", version.getVersionString()));
        requireNotNullArg(version, "PDF version cannot be null");
    }

    @Override
    public PdfVersion getVersion() {
        return this.key();
    }

    @Override
    public boolean isHigherOrEqual(PdfVersion version) {
        return this.key().getVersion() >= version.getVersion();
    }
}
