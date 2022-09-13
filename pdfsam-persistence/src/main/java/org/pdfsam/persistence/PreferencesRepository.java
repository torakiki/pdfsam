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

import static java.util.Objects.nonNull;
import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import java.io.IOException;
import java.util.Optional;
import java.util.prefs.Preferences;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * A DAO providing basic CRUD functionalities for {@link String} keys and <T> values.
 *
 * @author Andrea Vacondio
 *
 * @param <T>
 *            the type of the persisted values
 */
public class PreferencesRepository<T> implements Repository<T>{

    private final ObjectMapper mapper;
    private final Class<T> clazz;
    private final StringPreferencesRepository repo;

    public PreferencesRepository(String path, ObjectMapper mapper, Class<T> clazz) {
        requireNotNullArg(mapper, "Mapper cannot be null");
        requireNotNullArg(clazz, "Type class cannot be null");
        this.repo = new StringPreferencesRepository(path);
        this.mapper = mapper;
        this.clazz = clazz;
    }

    /**
     * @param key
     * @return the stored value or an empty optional
     * @throws PersistenceException
     *             if this node (or an ancestor) has been removed or if key contains the null control character, code point U+0000.
     * @see Preferences#get(String, String)
     */
    public Optional<T> get(String key) throws PersistenceException {
        try {
            var value = this.repo.get(key);
            if (value.isPresent()) {
                return ofNullable(mapper.readValue(value.get(), clazz));
            }
        } catch (IOException e) {
            throw new PersistenceException(String.format("Unable to get entity: [key '%s']", key), e);
        }
        return empty();
    }

    /**
     * Saves to the persistence layer the given value.
     *
     * @param key
     * @param entity
     *            the entity to persist. Can be null.
     * @throws PersistenceException
     *             if this node (or an ancestor) has been removed or if key contains the null control character, code point U+0000.
     * @see Preferences#put(String, String)
     */
    public void save(String key, T entity) throws PersistenceException {
        // fast fail
        requireNotBlank(key, "Key cannot be blank");
        try {
            if (nonNull(entity)) {
                this.repo.save(key, mapper.writeValueAsString(entity));
            } else {
                this.repo.delete(key);
            }
        } catch (IOException e) {
            throw new PersistenceException(String.format("Unable to store entity: [key '%s', entity '%s']", key, entity), e);
        }
    }

    public void delete(String key) throws PersistenceException {
        this.repo.delete(key);
    }

    public void clean() throws PersistenceException {
        this.repo.clean();
    }

}
