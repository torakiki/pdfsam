/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/09/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.model.ui;

import static java.util.Objects.requireNonNull;

/**
 * Holds data regarding the status of the main stage
 *
 * @author Andrea Vacondio
 */
public record StageStatus(double x, double y, double width, double height, StageMode mode) {
    public static final StageStatus NULL = new StageStatus(0, 0, 0, 0);

    public StageStatus {
        requireNonNull(mode);
    }

    public StageStatus(double x, double y, double width, double height) {
        this(x, y, width, height, StageMode.DEFAULT);
    }

}
