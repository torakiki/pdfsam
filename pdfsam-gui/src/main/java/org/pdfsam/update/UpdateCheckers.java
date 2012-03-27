/*
 * Created on 27/mar/2012
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

/**
 * Factory for update checkers
 * 
 * @author Andrea Vacondio
 * 
 */
public final class UpdateCheckers {
    private UpdateCheckers() {
        // nothing
    }

    /**
     * Factory method for a new {@link UpdateChecker} checking for updates over http connection
     * 
     * @param uri
     * @return the new instance
     */
    public static UpdateChecker newHttpUpdateChecker(String uri) {
        return new HttpUpdateChecker(uri);
    }
}
