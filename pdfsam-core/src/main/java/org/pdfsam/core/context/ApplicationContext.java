/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/09/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@pdfsam.org).
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

import io.reactivex.rxjava3.disposables.CompositeDisposable;
import javafx.application.ConditionalFeature;
import javafx.application.Platform;
import javafx.scene.Scene;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Key;
import org.pdfsam.persistence.PreferencesRepository;

import java.io.Closeable;
import java.util.Objects;
import java.util.Optional;

import static java.util.function.Predicate.not;
import static org.pdfsam.core.context.StringPersistentProperty.FONT_SIZE;

/**
 * @author Andrea Vacondio
 */
public class ApplicationContext implements Closeable {

    private static final ApplicationContext CONTEXT = new ApplicationContext();

    private final ApplicationPersistentSettings persistentSettings;
    private ApplicationRuntimeState runtimeState;
    private Optional<Injector> injector = Optional.empty();
    private CompositeDisposable disposable = new CompositeDisposable();

    private ApplicationContext() {
        this(new ApplicationPersistentSettings(new PreferencesRepository("/org/pdfsam/user/conf")), null);
    }

    /**
     * @deprecated use in tests
     */
    @Deprecated
    ApplicationContext(ApplicationPersistentSettings persistentSettings, ApplicationRuntimeState runtimeState) {
        this.persistentSettings = persistentSettings;
        this.runtimeState = runtimeState;
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
        synchronized (this) {
            if (Objects.isNull(this.runtimeState)) {
                this.runtimeState = new ApplicationRuntimeState();
            }
        }
        return this.runtimeState;
    }

    /**
     * Register the given scene to application context to listen to theme changes and other events
     *
     * @param scene
     */
    public void registerScene(Scene scene) {
        disposable.add(this.runtimeState().theme().subscribe(t -> {
            scene.getStylesheets().setAll(t.stylesheets());
            if (!Platform.isSupported(ConditionalFeature.TRANSPARENT_WINDOW)) {
                scene.getStylesheets().addAll(t.transparentIncapableStylesheets());
            }
        }));
        disposable.add(this.persistentSettings().settingsChanges(FONT_SIZE).subscribe(size -> {
            size.filter(StringUtils::isNotBlank).map(s -> String.format("-fx-font-size: %s;", s))
                    .ifPresentOrElse(scene.getRoot()::setStyle, () -> scene.getRoot().setStyle(""));
        }));

        this.persistentSettings().get(FONT_SIZE).filter(not(String::isBlank))
                .ifPresent(size -> scene.getRoot().setStyle(String.format("-fx-font-size: %s;", size)));
    }

    /**
     * Sets the injector
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

    public <T> T instance(Key<T> key) {
        return injector.orElseThrow(() -> new IllegalStateException("Injector not set for this application"))
                .instance(key);
    }

    public void clean() {
        persistentSettings.clean();
    }

    @Override
    public void close() {
        injector.ifPresent(Injector::close);
        runtimeState().close();
        persistentSettings.close();
        disposable.dispose();
    }

}
