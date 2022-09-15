/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2012
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
package org.pdfsam.model.tool;

import org.pdfsam.i18n.I18nContext;

/**
 * The category for a tool
 *
 * @author Andrea Vacondio
 */
public enum ToolCategory {
    SPLIT(I18nContext.i18n().tr("Split")),
    MERGE(I18nContext.i18n().tr("Merge")),
    SECURITY(I18nContext.i18n().tr("Security")),
    OTHER(I18nContext.i18n().tr("Other"));

    private String description;

    ToolCategory(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

}
