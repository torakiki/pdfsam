/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/09/22
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
package org.pdfsam.core.context;

import org.pdfsam.injector.Injector;
import org.pdfsam.persistence.PreferencesRepository;

import java.io.Closeable;
import java.util.List;
import java.util.Optional;

/**
 * @author Andrea Vacondio
 */
public class ApplicationContext implements Closeable {

    private static final ApplicationContext CONTEXT = new ApplicationContext();

    private final ApplicationPersistentSettings persistentSettings;
    private final ApplicationRuntimeState runtimeState;
    private Optional<Injector> injector = Optional.empty();

    private ApplicationContext() {
        this.persistentSettings = new ApplicationPersistentSettings(new PreferencesRepository("/org/pdfsam/user/conf"));
        this.runtimeState = new ApplicationRuntimeState();
    }

    /**
     * @return the application context
     */
    public static ApplicationContext app() {
        return CONTEXT;
    }

    /**
     * @return the application settings
     */
    public ApplicationPersistentSettings persistentSettings() {
        return this.persistentSettings;
    }

    /**
     * @return the application runtime state
     */
    public ApplicationRuntimeState runtimeState() {
        return this.runtimeState;
    }

    /**
     * Sets the injector
     *
     * @param injector
     */
    public void injector(Injector injector) {
        this.injector = Optional.ofNullable(injector);
    }

    /**
     * @return an instance of type
     */
    public <T> T instance(Class<T> type) {
        return injector.orElseThrow(() -> new IllegalStateException("Injector not set for this application"))
                .instance(type);
    }

    public <T> List<T> instancesOfType(Class<T> type) {
        return injector.orElseThrow(() -> new IllegalStateException("Injector not set for this application"))
                .instancesOfType(type);
    }

    public void clean() {
        persistentSettings.clean();
    }

    @Override
    public void close() {
        injector.ifPresent(Injector::close);
        runtimeState.close();
        persistentSettings.close();
    }

}
