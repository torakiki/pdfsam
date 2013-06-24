/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2012
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
package org.pdfsam.module;

import javax.swing.Icon;

/**
 * Metadata to describe a module.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class ModuleDescriptor {

    private String id;
    private ModuleCategory category;
    private String name;
    private Icon icon;

    public ModuleDescriptor(String id, ModuleCategory category, String name, Icon icon) {
        this.id = id;
        this.category = category;
        this.name = name;
        this.icon = icon;
    }

    public ModuleCategory getCategory() {
        return category;
    }

    public String getName() {
        return name;
    }

    public Icon getIcon() {
        return icon;
    }

    public String getId() {
        return id;
    }

}
