package org.pdfsam.model.ui;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 03/10/22
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

import org.apache.commons.lang3.StringUtils;

import java.util.Locale;
import java.util.Objects;

/**
 * A combo item with description
 *
 * @author Andrea Vacondio
 */
public class ComboItem<T> {

    private final T key;
    private final String description;

    public ComboItem(T key, String description) {
        this.key = key;
        this.description = description;
    }

    public T key() {
        return key;
    }

    public String description() {
        return description;
    }

    @Override
    public String toString() {
        return description;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(key);
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof ComboItem item)) {
            return false;
        }
        return Objects.equals(key, item.key());
    }

    public static ComboItem<String> fromLocale(Locale locale) {
        return new ComboItem<>(locale.toLanguageTag(), StringUtils.capitalize(locale.getDisplayName(locale)));
    }

    /**
     * Factory method for an item with no text representation
     */
    public static <K> ComboItem<K> keyWithEmptyValue(K key) {
        return new ComboItem<>(key, "");
    }
}
