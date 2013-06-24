/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
