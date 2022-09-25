package org.pdfsam.persistence;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/09/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import java.util.Optional;

/**
 * A DAO providing basic CRUD functionalities for {@link String} keys and <T> values.
 *
 * @author Andrea Vacondio
 */
public interface EntityRepository<T> extends Repository {

    /**
     * @return the entity corresponding to the given key or an empty {@link Optional}
     */
    Optional<T> get(String key);

    /**
     * @return the entity corresponding to the given key or the provided default value
     */
    T get(String key, T defaultValue);

    /**
     * Saves to the persistence layer the given entity.
     *
     * @param entity the entity to save. A null entity will delete the existing persisted value.
     * @throws PersistenceException if there is an error serializing or storing the entity
     */
    void save(String key, T entity);
}
