/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13 sep 2022
 * Copyright 2022 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.persistence;

import java.util.function.Supplier;

/**
 * A DAO providing basic CRUD functionalities for {@link String} keys and values.
 *
 * @author Andrea Vacondio
 */
public interface Repository {

    /**
     * @return the stored value converted to Integer or the provided default
     */
    int getInt(String key, int defaultValue);

    /**
     * @return the stored value converted to Integer or the default provided by the given {@link Supplier}
     */
    default int getInt(String key, Supplier<Integer> supplier) {
        return this.getInt(key, supplier.get());
    }

    /**
     * @return the stored value converted to Long or the provided default
     */
    long getLong(String key, long defaultValue);

    /**
     * @return the stored value converted to Long or the default provided by the given {@link Supplier}
     */
    default long getLong(String key, Supplier<Long> supplier) {
        return this.getLong(key, supplier.get());
    }

    /**
     * @return the stored value converted to String or the provided default
     */
    default String getString(String key, String defaultValue) {
        return this.getString(key, () -> defaultValue);
    }

    /**
     * @return the stored value converted to String or the default provided by the given {@link Supplier}
     */
    String getString(String key, Supplier<String> supplier);

    /**
     * @return the stored value converted to boolean or the provided default
     */
    boolean getBoolean(String key, boolean defaultValue);

    /**
     * @return the stored value converted to boolean or the default provided by the given {@link Supplier}
     */
    default boolean getBoolean(String key, Supplier<Boolean> supplier) {
        return this.getBoolean(key, supplier.get());
    }

    /**
     * @throws PersistenceException if there is an error storing the value
     */
    void saveInt(String key, int value);

    /**
     * @throws PersistenceException if there is an error storing the value
     */
    void saveLong(String key, long value);

    /**
     * Saves to the persistence layer the given value.
     *
     * @param value the String to save. A null value will delete the existing persisted value.
     * @throws PersistenceException if there is an error storing the value
     */
    void saveString(String key, String value);

    /**
     * @throws PersistenceException if there is an error storing the value
     */
    void saveBoolean(String key, boolean value);

    /**
     * @return the array of keys stored in this repository
     * @throws PersistenceException if there is an error with the backing store
     */
    String[] keys();

    /**
     * Deletes the value corresponding to the given key
     *
     * @throws PersistenceException if there is an error deleting the value
     */
    void delete(String key);

    /**
     * Removes all the persisted values and keys for this repository
     *
     * @throws PersistenceException if this operation cannot be completed
     */
    void clean();
}
