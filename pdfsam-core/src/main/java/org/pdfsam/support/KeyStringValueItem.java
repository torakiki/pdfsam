/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/apr/2012
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

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * {@link KeyValueItem} where the value is a {@link String}. Two items with the same key are considered equals.
 * 
 * @author Andrea Vacondio
 * @param <K>
 *            type for the key
 */
public class KeyStringValueItem<K> implements KeyValueItem<K, String> {

    private K key;
    private String value;

    protected KeyStringValueItem(K key, String value) {
        requireNotNullArg(key, "Key cannot be null");
        this.key = key;
        this.value = value;
    }

    @Override
    public K getKey() {
        return key;
    }

    @Override
    public String getValue() {
        return value;
    }

    /**
     * Factory method for a key value instance where the value is empty
     * 
     * @param key
     * @return
     */
    public static <K> KeyStringValueItem<K> keyEmptyValue(K key) {
        return new KeyStringValueItem<>(key, EMPTY);
    }

    /**
     * Factory method for a key value instance
     * 
     * @param key
     * @param value
     * @return
     */
    public static <K> KeyStringValueItem<K> keyValue(K key, String value) {
        return new KeyStringValueItem<>(key, value);
    }

    @Override
    public String toString() {
        return value.toString();
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
        if (!(other instanceof KeyStringValueItem)) {
            return false;
        }
        KeyStringValueItem<?> item = (KeyStringValueItem<?>) other;
        return new EqualsBuilder().append(key, item.getKey()).isEquals();
    }

}
