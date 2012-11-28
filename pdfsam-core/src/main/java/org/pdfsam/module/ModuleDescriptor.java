/*
 * Created on 28/nov/2012
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
