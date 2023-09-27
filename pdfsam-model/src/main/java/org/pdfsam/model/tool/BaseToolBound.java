/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/09/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * Base tool bound class.
 *
 * @author Andrea Vacondio
 */
public class BaseToolBound implements ToolBound {

    private final String toolId;

    public BaseToolBound(String toolId) {
        requireNotBlank(toolId, "Bound tool id cannot be blank");
        this.toolId = toolId;
    }

    @Override
    public String toolBinding() {
        return toolId;
    }

}
