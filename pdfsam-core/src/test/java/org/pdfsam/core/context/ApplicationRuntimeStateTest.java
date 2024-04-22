/*
 * This file is part of the PDF Split And Merge source code
 * Created on 11 feb 2021
 * Copyright 2021 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.test.ValuesRecorder;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 */
@Execution(ExecutionMode.SAME_THREAD)
public class ApplicationRuntimeStateTest {

    private ApplicationRuntimeState victim;

    @BeforeEach
    public void setUp() {
        victim = new ApplicationRuntimeState();
    }

    @Test
    @DisplayName("Existing working Path")
    public void positiveMaybeWorkingPath(@TempDir Path tempDir) {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.maybeWorkingPath(tempDir);
        assertThat(values.values()).containsExactly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("Existing working path as String")
    public void positiveMaybeWorkingPathString(@TempDir Path tempDir) {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.maybeWorkingPath(tempDir.toString());
        assertThat(values.values()).containsExactly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("Null working Path")
    public void nullMaybeWorkingPath(@TempDir Path tempDir) {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.maybeWorkingPath(tempDir);
        victim.maybeWorkingPath((Path) null);
        assertThat(values.values()).containsExactly(empty(), of(tempDir), empty());
    }

    @Test
    @DisplayName("Null working path as String")
    public void nullMaybeWorkingPathString(@TempDir Path tempDir) {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.maybeWorkingPath(tempDir);
        victim.maybeWorkingPath((String) null);
        assertThat(values.values()).containsExactly(empty(), of(tempDir), empty());
    }

    @Test
    @DisplayName("Blank working path as String")
    public void blankMaybeWorkingPathString(@TempDir Path tempDir) {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.maybeWorkingPath(tempDir);
        victim.maybeWorkingPath("  ");
        assertThat(values.values()).containsExactly(empty(), of(tempDir), empty());
    }

    @Test
    @DisplayName("File working Path")
    public void fileMaybeWorkingPath(@TempDir Path tempDir) throws IOException {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.maybeWorkingPath(tempDir);
        var another = Files.createTempFile("test", ".tmp");
        victim.maybeWorkingPath(another);
        assertThat(values.values()).containsExactly(empty(), of(tempDir), of(another.getParent()));
    }

    @Test
    @DisplayName("Default working Path")
    public void defaultWorkingPath(@TempDir Path tempDir) {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.defaultWorkingPath(tempDir);
        assertThat(values.values()).containsExactly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("Non regular file")
    public void fileWorkingPath(@TempDir Path tempDir) {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.workingPath(tempDir);
        victim.workingPath(tempDir.resolve("test.tmp"));
        assertThat(values.values()).containsExactly(empty(), of(tempDir), empty());
    }

    @Test
    @DisplayName("File working Path with default already set")
    public void maybeWorkingPathWithDefault(@TempDir Path tempDir) throws IOException {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.defaultWorkingPath(tempDir);
        var another = Files.createTempFile(tempDir, "test", ".tmp");
        victim.maybeWorkingPath(another);
        assertThat(values.values()).containsExactly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("File working String with default already set")
    public void maybeWorkingPathStringWithDefault(@TempDir Path tempDir) throws IOException {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.defaultWorkingPath(tempDir);
        var another = Files.createTempFile(tempDir, "test", ".tmp");
        victim.maybeWorkingPath(another.toString());
        assertThat(values.values()).containsExactly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("Removing default working Path")
    public void removeWorkingPathWithDefault(@TempDir Path tempDir) throws IOException {
        var values = new ValuesRecorder<Optional<Path>>();
        victim.workingPath().subscribe(values);
        victim.defaultWorkingPath(tempDir);
        var another = Files.createTempFile(tempDir, "test", ".tmp");
        victim.maybeWorkingPath(another);
        assertThat(values.values()).containsExactly(empty(), of(tempDir));
        values.clear();
        victim.defaultWorkingPath(null);
        victim.maybeWorkingPath(another);
        assertThat(values.values()).containsExactly(empty(), of(another.getParent()));
    }

    @Test
    @DisplayName("Active tool notified")
    public void positiveActiveTool() {
        var values = new ValuesRecorder<Optional<Tool>>();
        victim.activeTool().subscribe(values);
        var tool = mock(Tool.class);
        victim.activeTool(tool);
        assertThat(values.values()).containsExactly(empty(), of(tool));
    }

    @Test
    @DisplayName("Null active tool")
    public void nullActiveTool() {
        var values = new ValuesRecorder<Optional<Tool>>();
        victim.activeTool().subscribe(values);
        var tool = mock(Tool.class);
        victim.activeTool(tool);
        victim.activeTool(null);
        assertThat(values.values()).containsExactly(empty(), of(tool), empty());
    }
}
