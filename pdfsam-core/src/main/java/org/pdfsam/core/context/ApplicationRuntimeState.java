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

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
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
import java.util.Objects;
import java.util.Optional;
import java.util.ServiceLoader;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;

/**
 * Application Runtime state
 *
 * @author Andrea Vacondio
 */
public class ApplicationRuntimeState {

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationRuntimeState.class);

    private Path defaultWorkingPath;
    private final SimpleObjectProperty<Optional<Path>> workingPath = new SimpleObjectProperty<>(empty());
    private final SimpleObjectProperty<Optional<Tool>> activeTool = new SimpleObjectProperty<>(empty());
    private final SimpleObjectProperty<Theme> theme = new SimpleObjectProperty<>();
    private final Future<Map<String, Tool>> tools;

    ApplicationRuntimeState() {
        try (var executor = Executors.newThreadPerTaskExecutor(Thread.ofVirtual().name("tools-loader-", 0).factory())) {
            this.tools = executor.submit(() -> ServiceLoader.load(Tool.class).stream().map(ServiceLoader.Provider::get)
                    .collect(toMap(Tool::id, identity())));
        }
    }

    /**
     * Sets the current working path for the application
     *
     * @param path the current working directory or the parent in case of regular file. A null value clears the current working path
     */
    void workingPath(Path path) {
        workingPath.set(ofNullable(path).map(p -> {
            if (Files.isRegularFile(p)) {
                return p.getParent();
            }
            return p;
        }).filter(Files::isDirectory));
    }

    /**
     * Sets the current working path for the application unless there is a default one already set.
     * This method does nothing in case there is a user defined default working path.
     *
     * @param path a valid path string. A blank or null or non directory value clears the current working path
     */
    public void maybeWorkingPath(String path) {
        this.maybeWorkingPath(ofNullable(path).filter(StringUtils::isNotBlank).map(Paths::get).orElse(null));
    }

    /**
     * Sets the current working path for the application unless there is a default one already set.
     * This method does nothing in case there is a user defined default working path.
     *
     * @param path the current working directory or the parent in case of regular file. A null value clears the current working path
     * @see #workingPath(Path)
     */
    public void maybeWorkingPath(Path path) {
        if (Objects.isNull(defaultWorkingPath)) {
            workingPath(path);
        }
    }

    /**
     * @return the current workingPath value
     */
    public Optional<Path> workingPathValue() {
        return workingPath.getValue();
    }

    public ObservableValue<Optional<Path>> workingPath() {
        return workingPath;
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
     * @param activeTool the tool currently active
     */
    public void activeTool(Tool activeTool) {
        this.activeTool.set(ofNullable(activeTool));
    }

    /**
     * @return the current active tool value
     */
    public Optional<Tool> activeToolValue() {
        return activeTool.getValue();
    }

    public ObservableValue<Optional<Tool>> activeTool() {
        return activeTool;
    }

    /**
     * @return the current theme
     */
    public ObservableValue<Theme> theme() {
        return this.theme;
    }

    /**
     * Sets the application theme
     */
    public void theme(Theme theme) {
        ofNullable(theme).ifPresent(this.theme::set);
    }

    void defaultWorkingPath(Path defaultWorkingPath) {
        this.defaultWorkingPath = defaultWorkingPath;
        workingPath(defaultWorkingPath);
    }

}
