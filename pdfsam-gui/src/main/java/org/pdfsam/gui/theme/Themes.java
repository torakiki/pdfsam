package org.pdfsam.gui.theme;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 06/10/22
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

import javafx.application.ColorScheme;
import javafx.application.Platform;
import org.pdfsam.theme.Theme;

import java.util.Collections;
import java.util.ServiceLoader;
import java.util.SortedMap;
import java.util.TreeMap;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;
import static org.sejda.commons.util.RequireUtils.require;

/**
 * @author Andrea Vacondio
 */
public class Themes {

    private static final TreeMap<String, Theme> THEMES = ServiceLoader.load(Theme.class).stream()
            .map(ServiceLoader.Provider::get).collect(toMap(Theme::id, identity(), (a, b) -> a, TreeMap::new));

    /**
     * @param id the theme id
     * @return the theme with the given id or a default theme if no theme with the given id is found
     * @throws IllegalStateException if no theme is available
     */
    public static Theme getOrDefault(String id) {
        return ofNullable(get(id)).orElseGet(Themes::defaultTheme);
    }

    /**
     * @param id the theme id
     * @return the theme with the given id or null
     */
    public static Theme get(String id) {
        if (nonNull(id)) {
            return THEMES.get(id);
        }
        return null;
    }

    private static Theme defaultTheme() {
        require(!THEMES.isEmpty(), () -> new IllegalStateException("No theme available"));
        ColorScheme colorScheme = Platform.getPreferences().getColorScheme();
        for (Theme theme : THEMES.values()) {
            if (theme.isDefault() && isSameScheme(theme, colorScheme)) {
                return theme;
            }
        }
        return THEMES.get(THEMES.firstKey());
    }

    private static boolean isSameScheme(Theme theme, ColorScheme colorScheme) {
        return theme.isDark() == (ColorScheme.DARK == colorScheme);
    }

    /**
     * @return an unmodifiable sorted view of the themes available for the application
     */
    public static SortedMap<String, Theme> themes() {
        return Collections.unmodifiableSortedMap(THEMES);
    }
}
