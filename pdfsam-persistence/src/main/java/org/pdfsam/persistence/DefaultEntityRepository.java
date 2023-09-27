/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13 sep 2022
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
package org.pdfsam.persistence;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.prefs.Preferences;

import static java.util.Objects.nonNull;
import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * A DAO providing basic CRUD functionalities for {@link String} keys and <T> values.
 *
 * @param <T> the type of the persisted values
 * @author Andrea Vacondio
 */
public class DefaultEntityRepository<T> implements EntityRepository<T> {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultEntityRepository.class);

    private final ObjectMapper mapper;
    private final Class<T> clazz;
    private final PreferencesRepository repo;

    public DefaultEntityRepository(String path, ObjectMapper mapper, Class<T> clazz) {
        requireNotNullArg(mapper, "Mapper cannot be null");
        requireNotNullArg(clazz, "Type class cannot be null");
        this.repo = new PreferencesRepository(path);
        this.mapper = mapper;
        this.clazz = clazz;
    }

    /**
     * @return the stored value or an empty optional
     * @throws PersistenceException if this node (or an ancestor) has been removed or if key contains the null control character, code point U+0000.
     * @see Preferences#get(String, String)
     */
    @Override
    public Optional<T> get(String key) throws PersistenceException {
        try {
            var value = ofNullable(this.repo.getString(key, (String) null));
            if (value.isPresent()) {
                return ofNullable(mapper.readValue(value.get(), clazz));
            }
        } catch (IOException e) {
            throw new PersistenceException(String.format("Unable to get entity: [key '%s']", key), e);
        }
        return empty();
    }

    @Override
    public T get(String key, T defaultValue) {
        return get(key).orElse(defaultValue);
    }

    @Override
    public int getInt(String key, int defaultValue) {
        return this.repo.getInt(key, defaultValue);
    }

    @Override
    public long getLong(String key, long defaultValue) {
        return this.repo.getLong(key, defaultValue);
    }

    @Override
    public String getString(String key, String defaultValue) {
        return this.repo.getString(key, defaultValue);
    }

    @Override
    public String getString(String key, Supplier<String> supplier) {
        return this.repo.getString(key, supplier);
    }

    @Override
    public boolean getBoolean(String key, boolean defaultValue) {
        return this.repo.getBoolean(key, defaultValue);
    }

    /**
     * Saves to the persistence layer the given value.
     *
     * @param key
     * @param entity the entity to persist. Can be null.
     * @throws PersistenceException if this node (or an ancestor) has been removed or if key contains the null control character, code point U+0000.
     * @see Preferences#put(String, String)
     */
    @Override
    public void save(String key, T entity) throws PersistenceException {
        // fast fail
        requireNotBlank(key, "Key cannot be blank");
        try {
            if (nonNull(entity)) {
                this.repo.saveString(key, mapper.writeValueAsString(entity));
            } else {
                this.repo.delete(key);
            }
        } catch (IOException e) {
            throw new PersistenceException(
                    String.format("Unable to store entity: [key '%s', entity '%s']", key, entity), e);
        }
    }

    @Override
    public void saveInt(String key, int value) throws PersistenceException {
        this.repo.saveInt(key, value);
    }

    @Override
    public void saveLong(String key, long value) throws PersistenceException {
        this.repo.saveLong(key, value);
    }

    @Override
    public void saveString(String key, String value) throws PersistenceException {
        this.repo.saveString(key, value);
    }

    @Override
    public void saveBoolean(String key, boolean value) {
        this.repo.saveBoolean(key, value);
    }

    @Override
    public String[] keys() {
        return this.repo.keys();
    }

    @Override
    public void delete(String key) throws PersistenceException {
        this.repo.delete(key);
    }

    @Override
    public void clean() throws PersistenceException {
        this.repo.clean();
    }

}
