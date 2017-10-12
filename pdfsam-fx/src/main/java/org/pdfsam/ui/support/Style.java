/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
package org.pdfsam.ui.support;

/**
 * Defines css style classes
 * 
 * @author Andrea Vacondio
 * 
 */
public enum Style {
    BUTTON("pdfsam-button"),
    BROWSE_BUTTON("pdfsam-button", "browse-button"),
    FOOTER_BUTTON("pdfsam-button", "footer-button"),
    TITLED_PANE("titled-pane"),
    DEAULT_CONTAINER("default-container"),
    CONTAINER("pdfsam-container"),
    MODULE_CONTAINER("pdfsam-module-container"),
    CLOSE_FOOTER("pdfsam-container", "pdfsam-footer-close-pane"),
    INVALID("invalid"),
    VITEM("spaced-vitem"),
    HCONTAINER("spaced-hcontainer"),
    MAIN_PANEL("main-scroll-panel"),
    GRID("pdfsam-grid"),
    WITH_HELP("with-help");

    public static final int DEFAULT_SPACING = 5;
    private String[] classes;

    private Style(String... classes) {
        this.classes = classes;
    }

    /**
     * @return an array of css classes
     */
    public String[] css() {
        return classes;
    }
}
