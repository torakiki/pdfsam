/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/feb/2014
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
package org.pdfsam.ui.io;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import java.io.File;

/**
 * Event to tell interested components what is the latest directory used by the application
 * 
 * @author Andrea Vacondio
 *
 */
public class SetLatestDirectoryEvent {

    private File latest;

    public SetLatestDirectoryEvent(File latest) {
        requireNotNullArg(latest, "Latest directory cannot be null");
        this.latest = latest;
    }

    public File getLatest() {
        return latest;
    }

}
