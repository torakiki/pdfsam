/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2013
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
package org.pdfsam.ui.commons;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.module.ModuleOwned;

/**
 * request to clear the module
 * 
 * @author Andrea Vacondio
 * 
 */
public class ClearModuleEvent implements ModuleOwned {

    private String ownerModule = StringUtils.EMPTY;
    public final boolean clearEverything;
    public final boolean askConfirmation;

    public ClearModuleEvent(String ownerModule, boolean clearEverything, boolean askConfirmation) {
        this.ownerModule = ownerModule;
        this.clearEverything = clearEverything;
        this.askConfirmation = askConfirmation;
    }

    public ClearModuleEvent(String ownerModule) {
        this(ownerModule, false, false);
    }

    @Override
    public String getOwnerModule() {
        return ownerModule;
    }
}
