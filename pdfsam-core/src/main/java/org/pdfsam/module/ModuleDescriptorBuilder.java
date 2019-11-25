/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/mag/2014
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
package org.pdfsam.module;

import static org.sejda.commons.util.RequireUtils.requireNotBlank;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Builder for the {@link ModuleDescriptor}
 * 
 * @author Andrea Vacondio
 *
 */
public final class ModuleDescriptorBuilder {

    private ModuleCategory category;
    private ModuleInputOutputType[] inputTypes;
    private String name;
    private String description;
    private int priority = ModulePriority.DEFAULT.getPriority();
    private String supportURL;

    private ModuleDescriptorBuilder() {
        // hide
    }

    public ModuleDescriptorBuilder category(ModuleCategory category) {
        this.category = category;
        return this;
    }

    public ModuleDescriptorBuilder inputTypes(ModuleInputOutputType... inputTypes) {
        this.inputTypes = inputTypes;
        return this;
    }

    public ModuleDescriptorBuilder name(String name) {
        this.name = name;
        return this;
    }

    public ModuleDescriptorBuilder description(String description) {
        this.description = description;
        return this;
    }

    public ModuleDescriptorBuilder priority(int priority) {
        this.priority = priority;
        return this;
    }

    public ModuleDescriptorBuilder priority(ModulePriority priority) {
        this.priority = priority.getPriority();
        return this;
    }

    public ModuleDescriptorBuilder supportURL(String supportURL) {
        this.supportURL = supportURL;
        return this;
    }

    /**
     * factory method
     * 
     * @return the builder instance
     */
    public static ModuleDescriptorBuilder builder() {
        return new ModuleDescriptorBuilder();
    }

    public ModuleDescriptor build() {
        requireNotNullArg(category, "Module category cannot be null");
        requireNotBlank(name, "Module name cannot be blank");
        requireNotBlank(description, "Module description cannot be blank");
        return new ModuleDescriptor(category, name, description, priority, supportURL, inputTypes);
    }
}
