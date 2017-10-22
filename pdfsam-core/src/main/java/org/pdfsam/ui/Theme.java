/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/ott/2014
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
package org.pdfsam.ui;

import java.util.Arrays;
import java.util.List;

import org.pdfsam.i18n.DefaultI18nContext;

/**
 * Themes available for the user to configure
 * 
 * @author Andrea Vacondio
 *
 */
public enum Theme {

    ROUNDISH {
        @Override
        public List<String> styleSheets() {
            return Arrays.asList("/themes/defaults.css", "/themes/progress.css", "/themes/pdfsam.css",
                    "/themes/news.css", "/themes/dialogs.css", "/themes/combo.css", "/themes/progress.css",
                    "/themes/banner.css", "/themes/quickbar.css", "/themes/notifications.css", "/themes/dashboard.css",
                    "/themes/menu.css", "/themes/defaults.last.css");
        }

        @Override
        public String friendlyName() {
            return DefaultI18nContext.getInstance().i18n("Roundish");
        }
    };

    public abstract List<String> styleSheets();

    public abstract String friendlyName();
}
