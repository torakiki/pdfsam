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

import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.subjects.BehaviorSubject;
import io.reactivex.rxjava3.subjects.ReplaySubject;
import javafx.application.ConditionalFeature;
import javafx.application.Platform;
import javafx.scene.Scene;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.theme.Theme;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.ServiceLoader;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;

/**
 * Application Runtime state
 *
 * @author Andrea Vacondio
 */
public class ApplicationRuntimeState implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationRuntimeState.class);

    private final BehaviorSubject<Optional<Path>> workingPath = BehaviorSubject.createDefault(empty());
    private final ReplaySubject<Theme> theme = ReplaySubject.create(1);
    private CompletableFuture<Map<String, Tool>> tools;

    ApplicationRuntimeState() {
        this.tools = CompletableFuture.supplyAsync(
                () -> ServiceLoader.load(Tool.class).stream().map(ServiceLoader.Provider::get)
                        .collect(toMap(Tool::id, identity()))).exceptionally(e -> {
            LOG.error("Unable to load tools list", e);
            return null;
        });
    }

    /**
     * Sets the current working path for the application
     *
     * @param path a valid path string. A blank or null or non directory value clears the current working path
     */
    public void workingPath(String path) {
        this.workingPath(ofNullable(path).filter(StringUtils::isNotBlank).map(Paths::get).orElse(null));

    }

    /**
     * Sets the current working path for the application
     *
     * @param path the current working directory or the path.parent in case of regular file. A null value clears the current working path
     */
    public void workingPath(Path path) {
        workingPath.onNext(ofNullable(path).map(p -> {
            if (Files.isRegularFile(p)) {
                return p.getParent();
            }
            return p;
        }).filter(Files::isDirectory));
    }

    public Observable<Optional<Path>> workingPath() {
        return workingPath.hide();
    }

    /**
     * @return the available tools
     */
    public Map<String, Tool> tools() {
        try {
            return this.tools.get();
        } catch (ExecutionException | InterruptedException e) {
            LOG.error("Unable to load tools list", e);
        }
        return Collections.emptyMap();
    }

    /**
     * @return the current theme
     */
    public Observable<Theme> theme() {
        return this.theme.hide();
    }

    /**
     * Sets the application scale
     */
    public void theme(Theme theme) {
        ofNullable(theme).ifPresent(this.theme::onNext);
    }

    /**
     * Subscribes the given scene to theme changes
     *
     * @param scene
     */
    public void subscribeThemedScene(Scene scene) {
        this.theme.subscribe(t -> {
            scene.getStylesheets().setAll(t.stylesheets());
            if (!Platform.isSupported(ConditionalFeature.TRANSPARENT_WINDOW)) {
                scene.getStylesheets().addAll(t.transparentIncapableStylesheets());
            }
        });
    }

    @Override
    public void close() {
        workingPath.onComplete();
        theme.onComplete();
    }
}
