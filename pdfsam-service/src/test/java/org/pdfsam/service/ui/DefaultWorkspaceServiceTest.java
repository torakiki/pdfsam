/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/dic/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.service.ui;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
public class DefaultWorkspaceServiceTest {

    private DefaultWorkspaceService victim;

    @BeforeEach
    public void setUp() {
        var mapper = JsonMapper.builder().addModule(new Jdk8Module()).addModule(new JavaTimeModule())
                .enable(DeserializationFeature.FAIL_ON_NUMBERS_FOR_ENUMS).enable(SerializationFeature.INDENT_OUTPUT)
                .disable(SerializationFeature.WRITE_DATE_TIMESTAMPS_AS_NANOSECONDS)
                .visibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)
                .serializationInclusion(JsonInclude.Include.NON_EMPTY)
                .build();
        victim = new DefaultWorkspaceService(mapper);
    }

    @Test
    public void saveNull() {
        assertThrows(IllegalArgumentException.class, () -> victim.saveWorkspace(Collections.emptyMap(), null));
    }

    @Test
    public void saveWorkspace(@TempDir Path folder) throws IOException {
        var file = Files.createTempFile(folder, null, null).toFile();
        assertFalse(Files.lines(file.toPath()).findAny().isPresent());
        Map<String, Map<String, String>> data = new HashMap<>();
        Map<String, String> moduleData = new HashMap<>();
        moduleData.put("key", "value");
        data.put("module", moduleData);
        victim.saveWorkspace(data, file);
        assertTrue(Files.lines(file.toPath()).findAny().isPresent());
    }

    @Test
    public void saveWorkspaceReadOnly(@TempDir Path folder) throws IOException {
        var file = Files.createTempFile(folder, null, null).toFile();
        file.setReadOnly();
        Map<String, Map<String, String>> data = new HashMap<>();
        Map<String, String> moduleData = new HashMap<>();
        moduleData.put("key", "value");
        data.put("module", moduleData);
        assertThrows(RuntimeException.class, () -> victim.saveWorkspace(data, file));
    }

    @Test
    public void loadNull() {
        assertThrows(IllegalArgumentException.class, () -> victim.loadWorkspace(null));
    }

    @Test
    public void loadWorkspace(@TempDir Path folder) throws IOException {
        var path = Files.createTempFile(folder, null, null);
        Files.copy(getClass().getResourceAsStream("/workspace.json"), path, StandardCopyOption.REPLACE_EXISTING);
        Map<String, Map<String, String>> result = victim.loadWorkspace(path.toFile());
        assertFalse(result.isEmpty());
        assertNotNull(result.get("split.bybookmarks"));
        assertFalse(result.get("split.bybookmarks").isEmpty());
    }

    @Test
    public void loadBrokenWorkspace(@TempDir Path folder) throws IOException {
        var path = Files.createTempFile(folder, null, null);
        Files.copy(getClass().getResourceAsStream("/broken_workspace.json"), path, StandardCopyOption.REPLACE_EXISTING);
        assertThrows(RuntimeException.class, () -> victim.loadWorkspace(path.toFile()));
    }

    @Test
    public void cannotAccessWorkspace(@TempDir Path folder) throws IOException {
        var path = Files.createTempFile(folder, null, null);
        Files.copy(getClass().getResourceAsStream("/workspace.json"), path, StandardCopyOption.REPLACE_EXISTING);
        var file = path.toFile();
        if (!file.setReadable(false)) {
            throw new RuntimeException("OS does not implement read pemissions");
        }
        assertThrows(RuntimeException.class, () -> victim.loadWorkspace(file));
    }

    @Test
    public void loadNotExistent() {
        File file = new File("I dont exist");
        assertThrows(RuntimeException.class, () -> victim.loadWorkspace(file));

    }

}
