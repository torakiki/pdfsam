/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/08/2025
 * Copyright 2025 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.model.ui.workspace;

import org.pdfsam.model.tool.ToolBound;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.EMPTY;

/**
 * @author Alessandro Parisi
 */
public class WorkspaceData {
    private final File file;
    private final Map<String, Map<String, String>> data;
    private boolean isRelativePaths = false;

    public WorkspaceData(File file, Map<String, Map<String, String>> data) {
        this.file = Objects.requireNonNull(file, "Workspace file cannot be null!");
        this.data = ofNullable(data).orElseGet(Hashtable::new);

        try {
            String s = getProperty("relative.paths");
            isRelativePaths = Boolean.parseBoolean(s);
        } catch (Exception ignored) {
        }
    }

    public String getProperty(String key) {
        return ofNullable(data.get(key)).map(Object::toString).orElse(null);
    }

    public ToolData getToolData(ToolBound tool) {
        return new ToolData(isRelativePaths ? file : null, data.get(tool.toolBinding()));
    }

    public File file() {
        return file;
    }

    public Map<String, Map<String, String>> data() {
        return Collections.unmodifiableMap(data);
    }

    public static class ToolData {
        private final File workspaceFile;
        private final Map<String, String> data;

        public ToolData() {
            this(null, new HashMap<>());
        }

        public ToolData(File workspaceFile, Map<String, String> data) {
            this.workspaceFile = workspaceFile;
            this.data = Optional.ofNullable(data).orElseGet(HashMap::new);
        }

        public String get(String key) {
            return data.get(key);
        }

        public String get(String key, String or) {
            return ofNullable(data.get(key)).orElse(or);
        }

        public Integer getInt(String key, Integer or) {
            return Optional.ofNullable(get(key)).map(Integer::parseInt).orElse(or);
        }

        public boolean getBoolean(String key) {
            return Optional.ofNullable(get(key)).map(Boolean::parseBoolean).orElse(false);
        }

        public Path getPath(String key) {
            Path toPath = Path.of(get(key, EMPTY));
            if (workspaceFile == null || Files.exists(toPath))
                return toPath;

            try {
                Path workspaceDir = workspaceFile.toPath().getParent();
                toPath = workspaceDir.resolve(toPath.getFileName());
                if (!Files.exists(toPath))
                    return Path.of("");
                return toPath;
            } catch (Exception ex) {
                return toPath;
            }
        }

        public void set(String key, String value) {
            data.put(key, value);
        }

        public void setBoolean(String key, boolean value) {
            data.put(key, Boolean.toString(value));
        }

        public void setInt(String key, int value) {
            data.put(key, Integer.toString(value));
        }

        public <E extends Enum<E>> void setEnum(String key, E value) {
            if (value != null)
                set(key, value.toString());
        }

        public boolean hasKey(String key) {
            return data.containsKey(key);
        }

        public boolean isEmpty() {
            return data.isEmpty();
        }
    }
}
