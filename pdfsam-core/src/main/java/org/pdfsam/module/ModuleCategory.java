/*
 * Created on 28/nov/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.module;

import org.pdfsam.context.DefaultI18nContext;

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
