/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08/mag/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.model.tool;

/**
 * Builder for the {@link ToolDescriptor}
 *
 * @author Andrea Vacondio
 */
public final class ToolDescriptorBuilder {

    private ToolCategory category;
    private ToolInputOutputType[] inputTypes;
    private String name;
    private String description;
    private int priority = ToolPriority.DEFAULT.getPriority();
    private String supportURL;

    private ToolDescriptorBuilder() {
        // hide
    }

    public ToolDescriptorBuilder category(ToolCategory category) {
        this.category = category;
        return this;
    }

    public ToolDescriptorBuilder inputTypes(ToolInputOutputType... inputTypes) {
        this.inputTypes = inputTypes;
        return this;
    }

    public ToolDescriptorBuilder name(String name) {
        this.name = name;
        return this;
    }

    public ToolDescriptorBuilder description(String description) {
        this.description = description;
        return this;
    }

    public ToolDescriptorBuilder priority(int priority) {
        this.priority = priority;
        return this;
    }

    public ToolDescriptorBuilder priority(ToolPriority priority) {
        this.priority = priority.getPriority();
        return this;
    }

    public ToolDescriptorBuilder supportURL(String supportURL) {
        this.supportURL = supportURL;
        return this;
    }

    /**
     * factory method
     *
     * @return the builder instance
     */
    public static ToolDescriptorBuilder builder() {
        return new ToolDescriptorBuilder();
    }

    public ToolDescriptor build() {
        return new ToolDescriptor(category, name, description, priority, supportURL, inputTypes);
    }
}
