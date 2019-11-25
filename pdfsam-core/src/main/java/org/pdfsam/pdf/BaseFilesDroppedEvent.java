/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30 ago 2019
 * Copyright 2019 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.pdf;

import static java.util.Optional.ofNullable;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

import java.io.File;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.module.ModuleOwned;

/**
 * A {@link File}s drop event to signal the user has dropped a file onto the the files selection component
 * 
 * @author Andrea Vacondio
 *
 */
public abstract class BaseFilesDroppedEvent implements ModuleOwned {

    public final List<File> files;
    private String ownerModule = StringUtils.EMPTY;

    public BaseFilesDroppedEvent(String ownerModule, List<File> files) {
        requireNotBlank(ownerModule, "Owner module cannot be blank");
        this.ownerModule = ownerModule;
        this.files = ofNullable(files).orElse(Collections.emptyList());
    }

    @Override
    public String getOwnerModule() {
        return ownerModule;
    }
}
