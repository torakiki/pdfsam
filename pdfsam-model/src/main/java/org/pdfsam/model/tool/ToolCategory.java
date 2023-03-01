/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2012
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
package org.pdfsam.model.tool;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * The category for a tool
 *
 * @author Andrea Vacondio
 */
public enum ToolCategory {
    SPLIT(i18n().tr("Split tools"), "category-split"),

    MERGE(i18n().tr("Merge tools"), "category-merge"),

    CONVERT(i18n().tr("Convert tools"), "category-convert"),

    EDIT(i18n().tr("Edit tools"), "category-edit"),

    OTHER(i18n().tr("Other tools"), "category-other");

    private final String description;
    private final String styleClass;

    ToolCategory(String description, String styleClass) {
        this.description = description;
        this.styleClass = styleClass;
    }

    public String getDescription() {
        return description;
    }

    /**
     * @return the style class for this category
     */
    public String styleClass() {
        return styleClass;
    }

}
