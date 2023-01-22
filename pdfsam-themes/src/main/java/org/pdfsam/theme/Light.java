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

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
public class Light implements Theme {

    @Override
    public String id() {
        return "AS876FDS7RB3";
    }

    @Override
    public List<String> stylesheets() {
        return List.of("/themes/light/colors.css", "/themes/light/theme.css", "/themes/light/tooltip.css",
                "/themes/light/progress.css", "/themes/light/list.css", "/themes/light/news.css",
                "/themes/light/dialogs.css", "/themes/light/combo.css", "/themes/light/scrollbars.css",
                "/themes/light/notifications.css", "/themes/light/dashboard.css", "/themes/light/menu.css",
                "/themes/light/table.css", "/themes/light/sidebar.css", "/themes/light/logs.css",
                "/themes/light/theme.last.css");
    }

    @Override
    public List<String> transparentIncapableStylesheets() {
        return List.of("/themes/light/transparent-incapable.css");
    }

    @Override
    public String name() {
        return i18n().tr("Light with green");
    }

    @Override
    public boolean isDark() {
        return false;
    }
}
