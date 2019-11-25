/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/nov/2013
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

import static org.sejda.commons.util.RequireUtils.requireNotBlank;

import java.util.Optional;

/**
 * Request to set the active the given module
 * 
 * @author Andrea Vacondio
 * 
 */
public final class SetActiveModuleRequest {
    private String id;

    private SetActiveModuleRequest(String id) {
        this.id = id;
    }

    public Optional<String> getActiveModuleId() {
        return Optional.ofNullable(id);
    }

    /**
     * @param id
     * @return an event to tell the content pane to activate the module with the given id in the Workarea panel.
     */
    public static SetActiveModuleRequest activeteModule(String id) {
        requireNotBlank(id, "Module id cannot be null");
        return new SetActiveModuleRequest(id);
    }

    /**
     * 
     * @return an event to tell the content pane to activate the Workarea (whatever module was previously active will be active module)
     */
    public static SetActiveModuleRequest activeteCurrentModule() {
        return new SetActiveModuleRequest(null);
    }
}
