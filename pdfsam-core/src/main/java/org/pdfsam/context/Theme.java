/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/ott/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.context;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Themes available for the user to configure
 * 
 * @author Andrea Vacondio
 *
 */
public enum Theme {
    GREEN {
        @Override
        public List<String> styleSheets() {
            return Arrays.asList("/themes/green/theme.css", "/themes/defaults.css", "/themes/pdfsam.css",
                    "/themes/notifications.css", "/themes/dashboard.css", "/themes/menu.css");
        }
    },
    CORNFLOWER {
        @Override
        public List<String> styleSheets() {
            return Arrays.asList("/themes/cornflower/theme.css", "/themes/defaults.css", "/themes/pdfsam.css",
                    "/themes/notifications.css", "/themes/dashboard.css", "/themes/menu.css");
        }
    },
    MATERIAL {
        @Override
        public List<String> styleSheets() {
            return Collections.emptyList();
        }
    };

    public abstract List<String> styleSheets();
}
