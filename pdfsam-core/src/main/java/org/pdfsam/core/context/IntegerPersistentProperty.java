/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10 gen 2021
 * Copyright 2021 by Sober Lemur S.r.l. (info@soberlemur.com).
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

/**
 * Configurable Integer value property
 *
 * @author Andrea Vacondio
 */
public enum IntegerPersistentProperty implements PersistentProperty<Integer> {
    LOGVIEW_ROWS_NUMBER(() -> 200);

    private final Supplier<Integer> defaultSupplier;

    IntegerPersistentProperty(Supplier<Integer> supplier) {
        this.defaultSupplier = supplier;
    }

    @Override
    public String key() {
        return this.name().toLowerCase();
    }

    @Override
    public Supplier<Integer> defaultSupplier() {
        return defaultSupplier;
    }
}
