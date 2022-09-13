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

import java.util.Optional;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * A DAO providing basic CRUD functionalities for {@link String} keys and {@link String} values.
 * 
 * @author Andrea Vacondio
 *
 */
public class StringPreferencesRepository implements Repository<String>{

    private static final Logger LOG = LoggerFactory.getLogger(StringPreferencesRepository.class);

    private final String path;

    public StringPreferencesRepository(String path) {
        requireNotBlank(path, "Preferences path cannot be blank");
        this.path = path;
    }

    /**
     * @param key
     * @return the stored value or an empty optional
     * @throws PersistenceException
     *             if this node (or an ancestor) has been removed or if key contains the null control character, code point U+0000.
     * @see Preferences#get(String, String)
     */
    public Optional<String> get(String key) throws PersistenceException {
        requireNotBlank(key, "Key cannot be blank");
        try {
            return ofNullable(Preferences.userRoot()
                                         .node(path)
                                         .get(key, null));
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to get value: [key '%s']", key), e);
        }
    }

    /**
     * Saves to the persistence layer the given value.
     * 
     * @param key
     * @param value
     *            the value. Can be null.
     * @throws PersistenceException
     *             if this node (or an ancestor) has been removed or if key contains the null control character, code point U+0000.
     * @see Preferences#put(String, String)
     */
    public void save(String key, String value) throws PersistenceException {
        requireNotBlank(key, "Key cannot be blank");
        try {
            if (nonNull(value)) {
                Preferences.userRoot()
                           .node(path)
                           .put(key, value);
                LOG.trace("Saved entity [key '{}', value '{}']", key, value);
            } else {
                delete(key);
            }
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to store value: [key '%s', value '%s']", key, value), e);
        }

    }

    /**
     * deletes the value corresponding to the given key
     * 
     * @param key
     * @throws PersistenceException
     *             if this node (or an ancestor) has been removed or if key contains the null control character, code point U+0000.
     * @see Preferences#remove(String)
     */
    public void delete(String key) throws PersistenceException {
        requireNotBlank(key, "Key cannot be blank");
        try {
            Preferences.userRoot()
                       .node(path)
                       .remove(key);
            LOG.trace("Deleted entity key '{}'", key);
        } catch (IllegalStateException | IllegalArgumentException e) {
            throw new PersistenceException(String.format("Unable to delete value: [key '%s']", key), e);
        }
    }

    /**
     * Removes all the persisted values and keys for this repository
     * 
     * @throws PersistenceException
     *             if this operation cannot be completed due to a failure in the backing store, or inability to communicate with it orif this node (or an ancestor) has already been
     *             removed with the removeNode() method.
     * @see Preferences#removeNode()
     */
    public void clean() throws PersistenceException {
        var prefs = Preferences.userRoot()
                               .node(path);
        try {
            prefs.removeNode();
            prefs.flush();
        } catch (IllegalStateException | BackingStoreException e) {
            throw new PersistenceException(String.format("Unable to clear preferences: [path '%s']", path), e);
        }
    }

}
