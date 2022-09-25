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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.function.Supplier;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * A DAO providing basic CRUD functionalities for {@link String} keys and {@link String} values.
 *
 * @author Andrea Vacondio
 */
public class PreferencesRepository implements Repository {

    private static final Logger LOG = LoggerFactory.getLogger(PreferencesRepository.class);

    final String path;

    public PreferencesRepository(String path) {
        requireNotBlank(path, "Preferences path cannot be blank");
        this.path = path;
    }

    @Override
    public int getInt(String key, int defaultValue) {
        try {
            return Preferences.userRoot().node(path).getInt(key, defaultValue);
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to get value: [key '%s']", key), e);
        }
    }

    @Override
    public long getLong(String key, long defaultValue) {
        try {
            return Preferences.userRoot().node(path).getLong(key, defaultValue);
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to get value: [key '%s']", key), e);
        }
    }

    @Override
    public String getString(String key, Supplier<String> supplier) {
        requireNotBlank(key, "Key cannot be blank");
        try {
            return ofNullable(Preferences.userRoot().node(path).get(key, null)).orElseGet(supplier);
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to get value: [key '%s']", key), e);
        }
    }

    @Override
    public boolean getBoolean(String key, boolean defaultValue) {
        try {
            return Preferences.userRoot().node(path).getBoolean(key, defaultValue);
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to get value: [key '%s']", key), e);
        }
    }

    @Override
    public void saveInt(String key, int value) {
        requireNotBlank(key, "Key cannot be blank");
        try {
            Preferences.userRoot().node(path).putInt(key, value);
            LOG.trace("Saved entity [key '{}', value '{}']", key, value);
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to store value: [key '%s', value '%s']", key, value),
                    e);
        }
    }

    @Override
    public void saveLong(String key, long value) {
        requireNotBlank(key, "Key cannot be blank");
        try {
            Preferences.userRoot().node(path).putLong(key, value);
            LOG.trace("Saved entity [key '{}', value '{}']", key, value);
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to store value: [key '%s', value '%s']", key, value),
                    e);
        }
    }

    @Override
    public void saveString(String key, String value) {
        requireNotBlank(key, "Key cannot be blank");
        try {
            if (nonNull(value)) {
                Preferences.userRoot().node(path).put(key, value);
                LOG.trace("Saved entity [key '{}', value '{}']", key, value);
            } else {
                delete(key);
            }
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to store value: [key '%s', value '%s']", key, value),
                    e);
        }
    }

    @Override
    public void saveBoolean(String key, boolean value) {
        requireNotBlank(key, "Key cannot be blank");
        try {
            Preferences.userRoot().node(path).putBoolean(key, value);
            LOG.trace("Saved entity [key '{}', value '{}']", key, value);
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to store value: [key '%s', value '%s']", key, value),
                    e);
        }
    }

    @Override
    public String[] keys() {
        try {
            return Preferences.userRoot().node(path).keys();
        } catch (IllegalStateException | BackingStoreException e) {
            throw new PersistenceException("Unable to retrieve key values", e);
        }
    }

    /**
     * deletes the value corresponding to the given key
     *
     * @param key
     * @throws PersistenceException if this node (or an ancestor) has been removed or if key contains the null control character, code point U+0000.
     * @see Preferences#remove(String)
     */
    @Override
    public void delete(String key) {
        requireNotBlank(key, "Key cannot be blank");
        try {
            Preferences.userRoot().node(path).remove(key);
            LOG.trace("Deleted entity key '{}'", key);
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to delete value: [key '%s']", key), e);
        }
    }

    /**
     * Removes all the persisted values and keys for this repository
     *
     * @throws PersistenceException if this operation cannot be completed due to a failure in the backing store, or inability to communicate with it orif this node (or an ancestor) has already been
     *                              removed with the removeNode() method.
     * @see Preferences#removeNode()
     */
    @Override
    public void clean() {
        var prefs = Preferences.userRoot().node(path);
        try {
            prefs.removeNode();
            prefs.flush();
        } catch (IllegalStateException | BackingStoreException e) {
            throw new PersistenceException(String.format("Unable to clear preferences: [path '%s']", path), e);
        }
    }

}
