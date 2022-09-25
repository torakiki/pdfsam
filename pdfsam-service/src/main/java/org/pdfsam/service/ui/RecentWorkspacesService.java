/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/dic/2014
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
package org.pdfsam.service.ui;

import java.io.File;
import java.util.List;

/**
 * Service dealing with the recently used workspaces.
 * 
 * @author Andrea Vacondio
 *
 */
public interface RecentWorkspacesService {

    /**
     * Adds the last used workspace to the collection of the recently used ones.
     * 
     * @param workspace
     */
    void addWorkspaceLastUsed(File workspace);

    /**
     * @return list of the absolute paths of the user's recently used workspaces
     */
    List<String> getRecentlyUsedWorkspaces();

    /**
     * Clear the recently used workspaces
     */
    void clear();
}
