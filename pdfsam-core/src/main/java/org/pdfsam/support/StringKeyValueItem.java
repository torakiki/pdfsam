/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/apr/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.support;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import static org.pdfsam.support.RequireUtils.require;

/**
 * Contains a pair of Strings, usually a key and a display value. Two items with the same key are considered equals.
 * 
 * @author Andrea Vacondio
 * 
 */
public class StringKeyValueItem {

    private String key;
    private String value;

    public StringKeyValueItem(String key, String value) {
        require(isNotBlank(key), "Key cannot be blank");
        this.key = key;
        this.value = value;
    }

    public String getKey() {
        return key;
    }

    public String getValue() {
        return value;
    }

    @Override
    public String toString() {
        return value;
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(key).toHashCode();
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof StringKeyValueItem)) {
            return false;
        }
        StringKeyValueItem item = (StringKeyValueItem) other;
        return new EqualsBuilder().append(key, item.getKey()).isEquals();
    }

}
