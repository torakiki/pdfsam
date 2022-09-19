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
package org.pdfsam.context;

import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.subjects.PublishSubject;
import io.reactivex.rxjava3.subjects.Subject;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.persistence.PersistenceException;
import org.pdfsam.persistence.StringPreferencesRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;

import static java.util.Optional.ofNullable;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Persistent settings for the application
 *
 * @author Andrea Vacondio
 */
public final class ApplicationPersistentSettings implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationPersistentSettings.class);

    private final StringPreferencesRepository repo;
    private final Subject<PersistentPropertyChange<String>> stringSettingsChanges = PublishSubject.create();
    private final Subject<PersistentPropertyChange<Integer>> intSettingsChanges = PublishSubject.create();
    private final Subject<PersistentPropertyChange<Boolean>> boolSettingsChanges = PublishSubject.create();

    ApplicationPersistentSettings(StringPreferencesRepository repo) {
        this.repo = repo;
    }

    /**
     * @param prop
     * @return the value for the given {@link StringPersistentProperty} or the default associated value
     */
    public Optional<String> get(StringPersistentProperty prop) {
        requireNotNullArg(prop, "Cannot get value for a null property");
        try {
            return ofNullable(this.repo.get(prop.key()).orElseGet((prop.defaultSupplier())));
        } catch (PersistenceException e) {
            LOG.error("Unable to get persistent property: " + prop, e);
        }
        return ofNullable(prop.defaultSupplier().get());
    }

    /**
     * @param prop
     * @return the value of the given {@link IntegerPersistentProperty} or the default associated value
     */
    public Optional<Integer> get(IntegerPersistentProperty prop) {
        requireNotNullArg(prop, "Cannot get value for a null property");
        try {
            return ofNullable(this.repo.get(prop.key()).filter(StringUtils::isNotBlank).map(Integer::valueOf)
                    .orElseGet(prop.defaultSupplier()));
        } catch (NumberFormatException | PersistenceException e) {
            LOG.error("Unable to get persistent property: " + prop, e);
        }
        return ofNullable(prop.defaultSupplier().get());
    }

    /**
     * @param prop
     * @return the value of the given {@link BooleanPersistentProperty} or the default associated value
     */
    public Optional<Boolean> get(BooleanPersistentProperty prop) {
        requireNotNullArg(prop, "Cannot get value for a null property");
        try {
            return ofNullable(this.repo.get(prop.key()).filter(StringUtils::isNotBlank).map(Boolean::valueOf)
                    .orElseGet(prop.defaultSupplier()));
        } catch (NumberFormatException | PersistenceException e) {
            LOG.error("Unable to get persistent property: " + prop, e);
        }
        return ofNullable(prop.defaultSupplier().get());
    }

    /**
     * Persists the given String property key/value
     */
    public void set(StringPersistentProperty prop, String value) {
        requireNotNullArg(prop, "Cannot set value for a null property");
        try {
            this.repo.save(prop.key(), value);
            stringSettingsChanges.onNext(new PersistentPropertyChange<>(prop, ofNullable(value)));
        } catch (PersistenceException e) {
            LOG.error("Unable to save persistent property", e);
        }

    }

    /**
     * Persists the given Integer property key/value
     */
    public void set(IntegerPersistentProperty prop, Integer value) {
        requireNotNullArg(prop, "Cannot set value for a null property");
        try {
            this.repo.save(prop.key(), ofNullable(value).map(Object::toString).orElse(null));
            intSettingsChanges.onNext(new PersistentPropertyChange<>(prop, ofNullable(value)));
        } catch (PersistenceException e) {
            LOG.error("Unable to save persistent property", e);
        }
    }

    /**
     * Persists the given Boolean property key/value
     */
    public void set(BooleanPersistentProperty prop, Boolean value) {
        requireNotNullArg(prop, "Cannot set value for a null property");
        try {
            this.repo.save(prop.key(), ofNullable(value).map(Object::toString).orElse(null));
            boolSettingsChanges.onNext(new PersistentPropertyChange<>(prop, ofNullable(value)));
        } catch (PersistenceException e) {
            LOG.error("Unable to save persistent property", e);
        }
    }

    /**
     * @return an observable for changes to the given property
     */
    public Observable<Optional<String>> settingsChanges(StringPersistentProperty prop) {
        return stringSettingsChanges.hide().filter(c -> c.property().equals(prop)).map(PersistentPropertyChange::value);
    }

    /**
     * @return an observable for changes to the given property
     */
    public Observable<Optional<Integer>> settingsChanges(IntegerPersistentProperty prop) {
        return intSettingsChanges.hide().filter(c -> c.property().equals(prop)).map(PersistentPropertyChange::value);
    }

    /**
     * @return an observable for changes to the given property
     */
    public Observable<Optional<Boolean>> settingsChanges(BooleanPersistentProperty prop) {
        return boolSettingsChanges.hide().filter(c -> c.property().equals(prop)).map(PersistentPropertyChange::value);
    }

    void clean() {
        try {
            this.repo.clean();
            LOG.info("Persistent application settings deleted");
        } catch (PersistenceException e) {
            LOG.error("Unable to clear application settings", e);
        }
    }

    @Override
    public void close() {
        stringSettingsChanges.onComplete();
        intSettingsChanges.onComplete();
        boolSettingsChanges.onComplete();
    }
}
