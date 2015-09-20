/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/ott/2014
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
package org.pdfsam.configuration;

import static org.pdfsam.support.RequireUtils.requireNotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.Theme;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import de.jensd.fx.glyphs.GlyphsStyle;

/**
 * Styles configuration for the application
 * 
 * @author Andrea Vacondio
 *
 */
public class StylesConfig {
    private static final Logger LOG = LoggerFactory.getLogger(StylesConfig.class);

    private List<String> styles = new ArrayList<>();

    public StylesConfig(Theme theme) {
        requireNotNull(theme, "Theme cannot be null");
        LOG.debug(DefaultI18nContext.getInstance().i18n("Installing theme {0}.", theme.friendlyName()));
        theme.styleSheets().stream().map(s -> this.getClass().getResource(s).toExternalForm()).forEach(styles::add);
        styles.add(this.getClass().getResource(GlyphsStyle.DEFAULT.getStylePath()).toExternalForm());
    }

    /**
     * @return a collection of styles configured for the application
     */
    public List<String> styles() {
        return Collections.unmodifiableList(styles);
    }
}
