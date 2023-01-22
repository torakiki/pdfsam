/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
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
package org.pdfsam.ui.components.support;

/**
 * Defines css style classes
 *
 * @author Andrea Vacondio
 */
public enum Style {
    BUTTON("btn"),
    BANNER_BUTTON("btn", "banner-btn"),
    BROWSE_BUTTON("btn", "browse-button"),
    FOOTER_BUTTON("btn", "footer-button"),
    SIDEBAR_BUTTON("btn", "sidebar-button"),
    NEWS_BUTTON("btn", "news-btn"),
    RUN_BUTTON("btn", "footer-button", "run-button"),
    TOOLBAR_BUTTON("toolbar-button"),
    TITLED_PANE("pdfsam-titled-pane"),
    DEAULT_CONTAINER("default-container"),
    CONTAINER("pdfsam-container"),
    MODULE_CONTAINER("pdfsam-module-container"),
    CLOSE_FOOTER("pdfsam-container", "pdfsam-footer-close-pane"),
    INVALID("invalid"),
    VITEM("spaced-vitem"),
    HCONTAINER("spaced-hcontainer"),
    VCONTAINER("spaced-vcontainer"),
    GRID("pdfsam-grid"),
    WITH_HELP("with-help");

    public static final int DEFAULT_SPACING = 5;
    private final String[] classes;

    Style(String... classes) {
        this.classes = classes;
    }

    /**
     * @return an array of css classes
     */
    public String[] css() {
        return classes;
    }
}
