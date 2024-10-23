/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28/giu/2013
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
package org.pdfsam.ui.components.selection.multiple.move;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Notifies that selected elements in the selection table should be moved
 *
 * @author Andrea Vacondio
 */
public record MoveSelectedRequest(MoveType type) {

    public MoveSelectedRequest {
        requireNotNullArg(type, "Type cannot be null");
    }

}
