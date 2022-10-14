/*
 * This file is part of the PDF Split And Merge source code
 * Created on 18/09/22
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
package org.pdfsam.core.context;

import java.util.function.Supplier;

import static org.pdfsam.core.ConfigurableSystemProperty.LOCALE_PROP;
import static org.pdfsam.core.ConfigurableSystemProperty.THEME_PROP;

/**
 * Configurable String value property
 *
 * @author Andrea Vacondio
 */
public enum StringPersistentProperty implements PersistentProperty<String> {
    WORKSPACE_PATH(() -> ""),
    WORKING_PATH(() -> ""),
    STARTUP_MODULE(() -> ""),
    LOCALE(() -> System.getProperty(LOCALE_PROP)),
    THEME(() -> System.getProperty(THEME_PROP, ""));

    private final Supplier<String> defaultSupplier;

    StringPersistentProperty(Supplier<String> supplier) {
        this.defaultSupplier = supplier;
    }

    @Override
    public String key() {
        return this.name().toLowerCase();
    }

    @Override
    public Supplier<String> defaultSupplier() {
        return defaultSupplier;
    }
}
