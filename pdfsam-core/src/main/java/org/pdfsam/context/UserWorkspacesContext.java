/*
 * Created on 08/apr/2012
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
package org.pdfsam.context;

import java.util.List;

/**
 * Provides methods to store and retrieve workspaces recently used by the user.
 * 
 * @author Andrea Vacondio
 * 
 */
public interface UserWorkspacesContext {

    /**
     * Adds a workspace to the collection of the recently used ones.
     * 
     * @param workspace
     */
    void addWorkspace(String workspace);

    /**
     * @return list of user's workspaces
     */
    List<String> getWorkspaces();
}
