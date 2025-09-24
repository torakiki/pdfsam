/*
 * This file is part of the PDF Split And Merge source code
 * Created on 23/09/2025
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

import java.io.File;
import java.util.Map;
import java.util.Objects;

import static java.util.stream.Collectors.toMap;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * Data class to keep track of the loaded workspace and its source file.
 *
 * @author Alessandro Parisi
 */
public record Workspace(Map<String, Map<String, String>> data, File file) {

    public void merge(Map<String, Map<String, String>> otherData) {
        if (Objects.nonNull(otherData)) {
            data.putAll(otherData);
        }
    }

    /**
     * Checks if this workspace's data is equal to the given data by verifying that all tools' data in the latter
     * are present here and that every key-value pair is the same.
     *
     * @see Map#equals(Object)
     */
    public boolean containsAllIgnoreEmpty(Map<String, Map<String, String>> otherData) {
        for (Map.Entry<String, Map<String, String>> e : otherData.entrySet()) {

            Map<String, String> toolData = data.get(e.getKey());
            if (toolData == null || !toolData.equals(
                    e.getValue().entrySet().stream().filter(entry -> isNotEmpty(entry.getValue()))
                            .collect(toMap(Map.Entry::getKey, Map.Entry::getValue)))) {
                return false;
            }
        }
        return true;
    }

    public Workspace withFile(File file) {
        return new Workspace(data, file);
    }
}
