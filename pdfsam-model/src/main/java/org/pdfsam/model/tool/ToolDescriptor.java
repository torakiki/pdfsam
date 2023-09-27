/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2012
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

import java.util.Arrays;
import java.util.List;

import static java.util.Objects.requireNonNull;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.defaultIfBlank;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * Metadata to describe a {@link Tool}.
 *
 * @param name        a human-readable, internationalized name for the tool
 * @param description a human-readable, internationalized description for the tool
 * @param priority    a rough indicator of the popularity of the tool. It can be used to present tools to the users in an order that has more chances of being
 *                    of interest for them.
 * @param supportUrl  an optional URL pointing at the location where a support resource can be found. This is intended to be a webpage where support material for the tool can be found
 * @author Andrea Vacondio
 */
public record ToolDescriptor(ToolCategory category, String name, String description, int priority, String supportUrl,
                             List<ToolInputOutputType> inputTypes) {

    public ToolDescriptor(ToolCategory category, String name, String description, int priority, String supportUrl,
            List<ToolInputOutputType> inputTypes) {
        requireNotBlank(name, "Tool name cannot be blank");
        requireNotBlank(description, "Tool description cannot be blank");
        this.category = requireNonNull(category);
        this.name = name;
        this.description = description;
        this.priority = priority;
        this.supportUrl = defaultIfBlank(supportUrl, null);
        this.inputTypes = requireNonNull(inputTypes);
    }

    ToolDescriptor(ToolCategory category, String name, String description, int priority, String supportUrl,
            ToolInputOutputType... inputTypes) {
        this(category, name, description, priority, supportUrl,
                ofNullable(inputTypes).filter(t -> t.length > 0).map(Arrays::asList)
                        .orElseGet(() -> List.of(ToolInputOutputType.OTHER)));
    }

    /**
     * @param type
     * @return true if this module has the given input type
     */
    public boolean hasInputType(ToolInputOutputType type) {
        return inputTypes.contains(type);
    }
}
