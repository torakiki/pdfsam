/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/09/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

package org.pdfsam.theme;

import java.util.List;

/**
 * @author Andrea Vacondio
 */
public interface Theme {
    /**
     * @return a unique identifier for this theme
     */
    String id();

    /**
     * @return a collection of stylesheets paths
     */
    List<String> stylesheets();

    /**
     * @return a collection of stylesheets paths to be loaded if the platform is transparent incapable
     */
    List<String> transparentIncapableStylesheets();

    /**
     * @return The theme name
     */
    String name();

    /**
     * @return if the theme is a dark theme
     */
    boolean isDark();
}
