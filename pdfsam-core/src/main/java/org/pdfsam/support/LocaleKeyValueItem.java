/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/giu/2013
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
package org.pdfsam.support;

import static org.pdfsam.support.RequireUtils.requireNotNull;

import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * {@link KeyValueItem} wrapped around a {@link Locale} instance holding key (tag) and value (displayName)
 * 
 * @author Andrea Vacondio
 * 
 */
public class LocaleKeyValueItem implements KeyValueItem<String, String>, Comparable<LocaleKeyValueItem> {

    private Locale locale;

    public LocaleKeyValueItem(Locale locale) {
        requireNotNull(locale, "Locale cannot be null");
        this.locale = locale;
    }

    @Override
    public String getKey() {
        return locale.toLanguageTag();
    }

    @Override
    public String getValue() {
        return StringUtils.capitalize(locale.getDisplayName());
    }

    @Override
    public String toString() {
        return getValue();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(getKey()).toHashCode();
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof LocaleKeyValueItem)) {
            return false;
        }
        LocaleKeyValueItem item = (LocaleKeyValueItem) other;
        return new EqualsBuilder().append(getKey(), item.getKey()).isEquals();
    }

    @Override
    public int compareTo(LocaleKeyValueItem o) {
        requireNotNull(o, "Could not compare null elements");
        return getValue().compareTo(o.getValue());
    }
}
