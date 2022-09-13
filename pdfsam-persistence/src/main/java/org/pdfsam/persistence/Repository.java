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

import java.util.Optional;

/**
 * A DAO providing basic CRUD functionalities for {@link String} keys and <T> values.
 *
 * @author Andrea Vacondio
 */
public interface Repository<T> {

    /**
     * @param key
     * @return the entity corresponding to the given key or an empty {@link Optional}
     * @throws PersistenceException
     */
    Optional<T> get(String key) throws PersistenceException;

    /**
     * Saves to the persistence layer the given entity.
     *
     * @param key
     * @param entity the entity to save. Can be null.
     * @throws PersistenceException if there is an error serializing or storing the entity
     */
    void save(String key, T entity) throws PersistenceException;

    /**
     * Deletes the entity corresponding to the given key
     *
     * @param key
     * @throws PersistenceException
     */
    void delete(String key) throws PersistenceException;

    /**
     * Removes all the persisted values and keys for this repository
     *
     * @throws PersistenceException if this operation cannot be completed
     */
    void clean() throws PersistenceException;
}
