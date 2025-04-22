/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/09/22
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
package org.pdfsam.core.context;

import javafx.application.ConditionalFeature;
import javafx.application.Platform;
import javafx.scene.Scene;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Key;
import org.pdfsam.persistence.PreferencesRepository;

import java.io.Closeable;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.Objects;
import java.util.Optional;

import static java.util.Optional.of;
import static java.util.Optional.ofNullable;
import static java.util.function.Predicate.not;
import static org.pdfsam.core.context.StringPersistentProperty.FONT_SIZE;
import static org.pdfsam.core.context.StringPersistentProperty.WORKING_PATH;
import static org.pdfsam.core.support.params.ConversionUtils.toWeb;

/**
 * @author Andrea Vacondio
 */
public class ApplicationContext implements Closeable {

    private static final ApplicationContext CONTEXT = new ApplicationContext();

    private final ApplicationPersistentSettings persistentSettings;
    private ApplicationRuntimeState runtimeState;
    private Optional<Injector> injector = Optional.empty();

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
                //listen for changes in the working path
                this.persistentSettings().settingsChanges(WORKING_PATH).subscribe(path -> {
                    this.runtimeState.defaultWorkingPath(
                            path.filter(StringUtils::isNotBlank).map(Paths::get).filter(Files::isDirectory)
                                    .orElse(null));
                });

                var workingPath = persistentSettings().get(WORKING_PATH).filter(StringUtils::isNotBlank).map(Paths::get)
                        .filter(Files::isDirectory).orElse(null);
                this.runtimeState.defaultWorkingPath(workingPath);
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
        this.runtimeState().theme().subscribe(t -> {
            if (Objects.nonNull(t)) {
                Platform.runLater(() -> {
                    scene.getStylesheets().setAll(t.stylesheets());
                    if (!Platform.isSupported(ConditionalFeature.TRANSPARENT_WINDOW)) {
                        scene.getStylesheets().addAll(t.transparentIncapableStylesheets());
                    }
                    ofNullable(t.defaultPrimary()).or(() -> of(toWeb(Platform.getPreferences().getAccentColor())))
                            .map(c -> String.format(".root{-default-primary: %s;}", c))
                            .map(css -> "data:text/css;base64," + Base64.getEncoder().encodeToString(css.getBytes()))
                            .ifPresent(scene.getStylesheets()::add);
                    this.persistentSettings().get(FONT_SIZE).filter(not(String::isBlank))
                            .ifPresent(size -> scene.getRoot().setStyle(String.format("-fx-font-size: %s;", size)));
                });
            }
        });
        this.persistentSettings().settingsChanges(FONT_SIZE).subscribe(size -> {
            size.filter(StringUtils::isNotBlank).map(s -> String.format("-fx-font-size: %s;", s))
                    .ifPresentOrElse(scene.getRoot()::setStyle, () -> scene.getRoot().setStyle(""));
        });
    }

    /**
     * Sets the injector
     */
    public void injector(Injector injector) {
        this.injector = ofNullable(injector);
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
    }

}
