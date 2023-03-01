/*
 * This file is part of the PDF Split And Merge source code
 * Created on 18/09/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@pdfsam.org).
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
 * Property that can be persisted and may have a default value.
 *
 * @author Andrea Vacondio
 */
public interface PersistentProperty<T> {
    /**
     * @return the key to store this property
     */
    String key();

    /**
     * @return default value provider, it must never be null
     */
    Supplier<T> defaultSupplier();
}
