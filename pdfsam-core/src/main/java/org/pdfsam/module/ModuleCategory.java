/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.module;

import org.pdfsam.i18n.DefaultI18nContext;

/**
 * The category for a module
 * 
 * @author Andrea Vacondio
 * 
 */
public enum ModuleCategory {
    SPLIT(DefaultI18nContext.getInstance().i18n("Split")),
    MERGE(DefaultI18nContext.getInstance().i18n("Merge")),
    SECURITY(DefaultI18nContext.getInstance().i18n("Security")),
    OTHER(DefaultI18nContext.getInstance().i18n("Other"));

    private String description;

    private ModuleCategory(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

}
