/*
 * Created on 06/apr/2012
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
package org.pdfsam.update;

import static org.pdfsam.support.RequireUtils.require;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

/**
 * Event to notify a new version available.
 * 
 * @author Andrea Vacondio
 * 
 */
public class UpdateAvailableEvent {

    private String availableVersion;

    public UpdateAvailableEvent(String availableVersion) {
        require(isNotBlank(availableVersion), "Available version cannot be empty");
        this.availableVersion = availableVersion;
    }

    public String getAvailableVersion() {
        return availableVersion;
    }

}
