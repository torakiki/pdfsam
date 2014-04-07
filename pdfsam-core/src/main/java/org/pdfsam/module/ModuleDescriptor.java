/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.module;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.support.RequireUtils.require;
import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Metadata to describe a module.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class ModuleDescriptor {

    private ModuleCategory category;
    private String name;
    private String description;
    private int priority = ModulePriority.DEFAULT.getPriority();

    public ModuleDescriptor(ModuleCategory category, String name, String description) {
        requireNotNull(category, "Module category cannot be null");
        require(isNotBlank(name), "Module name cannot be blank");
        require(isNotBlank(description), "Module description cannot be blank");
        this.category = category;
        this.name = name;
        this.description = description;
    }

    public ModuleDescriptor(ModuleCategory category, String name, String description, ModulePriority priority) {
        this(category, name, description, priority.getPriority());
    }

    public ModuleDescriptor(ModuleCategory category, String name, String description, int priority) {
        this(category, name, description);
        this.priority = priority;
    }

    public ModuleCategory getCategory() {
        return category;
    }

    /**
     * @return a human readable, internationalized name for the module
     */
    public String getName() {
        return name;
    }

    /**
     * @return a human readable, internationalized description for the module
     */
    public String getDescription() {
        return description;
    }

    /**
     * @return module priority. It is a rough indicator of the popularity of the module. It can be used to present modules to the users in an order that has more chances of being
     *         of interest for them. The idea is to use this value to present most commonly used modules on first.
     */
    public int getPriority() {
        return priority;
    }

}
